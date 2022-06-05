from __future__ import annotations

import math

from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from decimal import Decimal
from typing import List

from flask import Blueprint, render_template, request

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.currency import Points, Shares
from hanson.models.account import MarketAccount
from hanson.models.market import Market
from hanson.models.outcome import Outcome, Outcomes
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user
from hanson.models.probability import ProbabilityDistribution
from hanson.models.transaction import create_transaction_execute_order
from hanson.models.history import ProbabilityHistory
from hanson.graph import render_graph

app = Blueprint(name="market", import_name=__name__)


@app.get("/markets")
@with_tx
def route_get_markets(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    markets_with_caps = Market.list_all_with_capitalization(tx)
    return Response.ok_html(
        render_template(
            "market_index.html",
            session_user=session_user,
            markets_with_caps=markets_with_caps,
        )
    )


@app.get("/market/new")
@with_tx
def route_get_market_new(tx: Transaction) -> Response:
    _session_user = get_session_user(tx)
    return Response.internal_error("TODO: Implement new market page.")


@app.post("/market/new")
@with_tx
def route_post_market_new(tx: Transaction) -> Response:
    _session_user = get_session_user(tx)
    return Response.internal_error("TODO: Implement post new market page.")


@app.get("/market/<int:market_id>")
@with_tx
def route_get_market(tx: Transaction, market_id: int) -> Response:
    now = datetime.now(timezone.utc)
    session_user = get_session_user(tx)

    market = Market.get_by_id(tx, market_id)
    if market is None:
        return Response.not_found("This market does not exist.")

    author = User.get_by_id(tx, market.author_user_id)
    assert author is not None

    outcomes = Outcome.get_all_by_market(tx, market_id)

    # TODO: We might make a single query that gets the pool balances.
    points_account = MarketAccount.expect_points_account(tx, market_id)
    pool_accounts = [
        MarketAccount.expect_pool_account(tx, market_id, outcome.id)
        for outcome in outcomes.outcomes
    ]

    # Probabilities are proportional to exp(-pool_balance) per share, for the
    # logarithmic market scoring rule.
    numers = [math.exp(-pool_account.balance.amount) for pool_account in pool_accounts]
    denom = sum(numers)
    ps = [x / denom for x in numers]


    graph_range = request.args.get("graph_range") or "90d"

    if graph_range.endswith("d") and graph_range[:-1].isdigit():
        num_days = int(graph_range[:-1])
        graph_start_time = now - timedelta(days=num_days)

    elif graph_range.endswith("h") and graph_range[:-1].isdigit():
        num_hours = int(graph_range[:-1])
        graph_start_time = now - timedelta(days=num_hours)

    elif graph_range == "all":
        graph_start_time = market.created_at

    else:
        return Response.bad_request(
            "Invalid value for graph_range, must be 'all', days (e.g. '90d'), "
            "or hours (e.g. '12h')."
        )

    graph_start_time = max(market.created_at, graph_start_time)
    graph_duration = now - graph_start_time

    # Some time intervals, in minutes, that are nice when the width of a bar on
    # the bar chart is a multiple of them.
    human_friendly_interval_minutes = (
        1,
        2,
        5,
        10,
        15,
        30,
        60,
        2 * 60,
        3 * 60,
        4 * 60,
        6 * 60,
        8 * 60,
        12 * 60,
    )
    # I want at most 40 bars, so check which multiple of the time interval can
    # fit the most of them. Fall back to a multiple of 1 day.
    bin_size = timedelta(days=1 + graph_duration.total_seconds() // (3600 * 24 * 40) * (3600 * 24))

    for bar_duration_minutes in human_friendly_interval_minutes:
        num_bars = graph_duration.total_seconds() / (60 * bar_duration_minutes)
        if num_bars < 42:
            bin_size = timedelta(minutes=bar_duration_minutes)
            break

    ps_history = ProbabilityHistory.for_market(
        tx,
        market_id=market_id,
        bin_size=bin_size,
    )
    graph = render_graph(
        ps_history=ps_history,
        outcomes=outcomes.outcomes,
        start_time=graph_start_time,
        end_time=now,
    )

    return Response.ok_html(
        render_template(
            "market.html",
            session_user=session_user,
            market=market,
            author=author,
            outcomes=outcomes,
            probabilities=ps,
            capitalization=points_account.balance,
            graph=graph,
            zip=zip,
        )
    )


@dataclass(frozen=True)
class OrderDetails:
    market: Market
    outcomes: Outcomes

    # The probability distribution before executing the order.
    pd_before: ProbabilityDistribution

    # The probability distribution after executing the order.
    pd_after: ProbabilityDistribution

    # The target probability distribution, which may differ from `pd_after`
    # when the user doesn't want to spend so many points to achieve `pd_after`.
    pd_target: ProbabilityDistribution

    # The cost (to the user) of moving the probability distribution from
    # `pd_before` to `pd_after`. To execute the order, the cost should be
    # subtracted from the user's shares accounts, and added to the market's pool
    # accounts. Note that the cost may be negative!
    costs_shares: List[Decimal]

    # The number of points we need to exchange for outcome shares to be able to
    # trade against the pool.
    cost_points: Points

    # The maximum number of points that the user wanted to spend.
    max_spend: Points

    @staticmethod
    def from_request(tx: Transaction, market_id: int) -> OrderDetails | Response:
        """
        Extract order details from the request (either through GET or POST),
        correct the amounts if needed to normalize the distribution and to stay
        below the max spend, then return
        """
        from flask import request

        market = Market.get_by_id(tx, market_id)
        if market is None:
            return Response.not_found("This market does not exist.")

        try:
            v = request.args.get("max_spend") or request.form.get("max_spend") or "0.00"
            max_spend = Points(Decimal(v))
        except ValueError:
            return Response.bad_request("Invalid max_spend amount.")

        if max_spend <= Points.zero():
            return Response.bad_request("Invalid max_spend amount, must be positive.")

        outcomes = Outcome.get_all_by_market(tx, market_id)

        pool_accounts = [
            MarketAccount.expect_pool_account(tx, market_id, outcome.id)
            for outcome in outcomes.outcomes
        ]

        # Get the user's probabilities from the query parameters. There is nothing
        # that forces the user to enter a normalized probability distribution, but
        # we normalize it when we construct the `ProbabilityDistribution` later.
        raw_user_probabilities = []
        for outcome in outcomes.outcomes:
            k = f"outcome{outcome.id}"
            v = request.args.get(k) or request.form.get(k) or "0.0"
            try:
                raw_user_probabilities.append(float(v))
            except ValueError:
                raw_user_probabilities.append(1e-10)

        pd_before = ProbabilityDistribution.from_pool_balances(
            [a.balance for a in pool_accounts]
        )
        pd_target = ProbabilityDistribution.from_probabilities(raw_user_probabilities)
        costs = pd_before.cost_for_update(pd_target)

        # Round the costs to two decimals, because points have only two decimals.
        # Round the costs up, so what the user pays is maximum, and what the user
        # receives (a negative cost) is minimum. This ensures that we avoid
        # pathological cases where the user can make 0.01 point profit due to
        # rounding, and repeat that to steal all the points.
        costs = [math.ceil(x * 100) / Decimal('100.00') for x in costs]

        # The "costs" are the changes in the pool balances. But we assume the user
        # right now doesn't have any shares. (TODO: Take current balance into account.)
        # So any shares we want to put in the pool, we have to first create by
        # exchanging points for shares.
        cost = Points(max(*costs))

        pd_after = pd_target
        if cost > max_spend:
            t = max_spend.amount / cost.amount
            pd_after = pd_before.interpolate(pd_target, t)
            costs = pd_before.cost_for_update(pd_after)
            costs = [math.ceil(x * 100) / Decimal('100.00') for x in costs]
            cost = Points(max(*costs))

        return OrderDetails(
            market=market,
            outcomes=outcomes,
            pd_before=pd_before,
            pd_after=pd_after,
            pd_target=pd_target,
            costs_shares=costs,
            cost_points=cost,
            max_spend=max_spend,
        )


@app.get("/market/<int:market_id>/checkout")
@with_tx
def route_get_market_checkout(tx: Transaction, market_id: int) -> Response:
    session_user = get_session_user(tx)

    order = OrderDetails.from_request(tx, market_id)
    if isinstance(order, Response):
        return order

    # TODO: Warn if the cost is more than the user's balance.

    return Response.ok_html(
        render_template(
            "market_checkout.html",
            session_user=session_user,
            order=order,
            zip=zip,
        )
    )


@app.post("/market/<int:market_id>/checkout")
@with_tx
def route_post_market_checkout(tx: Transaction, market_id: int) -> Response:
    session_user = get_session_user(tx)

    order = OrderDetails.from_request(tx, market_id)
    if isinstance(order, Response):
        return order

    create_transaction_execute_order(
        tx,
        debit_user_id=session_user.user.id,
        credit_market_id=market_id,
        cost=order.cost_points,
        amounts=[
            Shares(cost, outcome_id=outcome.id)
            for outcome, cost in zip(order.outcomes.outcomes, order.costs_shares)
        ],
    )

    # TODO: Warn if the cost is more than the user's balance.
    tx.commit()

    # TODO: Include some kind of "your purchase was successful screen.
    return Response.redirect_see_other(f"/market/{market_id}")
