import math

from decimal import Decimal
from typing import List

from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.currency import Points, Shares
from hanson.models.account import MarketAccount
from hanson.models.market import Market
from hanson.models.outcome import Outcome
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user
from hanson.models.probability import ProbabilityDistribution

app = Blueprint(name="market", import_name=__name__)


@app.get("/markets")
@with_tx
def route_get_markets(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    markets = Market.list_all(tx)
    return Response.ok_html(
        render_template(
            "market_index.html",
            session_user=session_user,
            markets=markets,
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

    return Response.ok_html(
        render_template(
            "market.html",
            session_user=session_user,
            market=market,
            author=author,
            outcomes=outcomes,
            probabilities=ps,
            capitalization=points_account.balance,
            zip=zip,
        )
    )


@app.get("/market/<int:market_id>/checkout")
@with_tx
def route_get_market_checkout(tx: Transaction, market_id: int) -> Response:
    from flask import request

    session_user = get_session_user(tx)

    market = Market.get_by_id(tx, market_id)
    if market is None:
        return Response.not_found("This market does not exist.")

    try:
        v = request.args.get("maxspend") or "0.00"
        max_spend = Points(Decimal(v))
    except:
        return Response.bad_request("Invalid maxspend amount.")

    if max_spend <= Points.zero():
        return Response.bad_request("Invalid maxspend amount, must be positive.")

    outcomes = Outcome.get_all_by_market(tx, market_id)

    points_account = MarketAccount.expect_points_account(tx, market_id)
    pool_accounts = [
        MarketAccount.expect_pool_account(tx, market_id, outcome.id)
        for outcome in outcomes.outcomes
    ]

    # Get the user's probabilities from the query parameters. There is nothing
    # that forces the user to enter a normalized probability distribution, but
    # we normalize it when we construct the `ProbabilityDistribution` later.
    raw_user_probabilities = []
    for outcome in outcomes.outcomes:
        v = request.args.get(f"outcome{outcome.id}") or "0.0"
        try:
            raw_user_probabilities.append(float(v))
        except:
            raw_user_probabilities.append(1e-10)

    pd_before = ProbabilityDistribution.from_pool_balances(
        [a.balance for a in pool_accounts]
    )
    pd_target = ProbabilityDistribution.from_probabilities(raw_user_probabilities)
    costs = pd_before.cost_for_update(pd_target)

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
        cost = Points(max(*costs))

    return Response.ok_html(
        render_template(
            "market_checkout.html",
            session_user=session_user,
            market=market,
            outcomes=outcomes,
            pd_before=pd_before,
            pd_after=pd_after,
            costs=costs,
            cost=cost,
            zip=zip,
        )
    )
