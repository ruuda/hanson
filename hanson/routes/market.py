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
        v = request.args.get("maxspend")
        max_spend = Points(Decimal(v))
    except:
        return Response.bad_request("Invalid maxspend amount.")

    outcomes = Outcome.get_all_by_market(tx, market_id)

    points_account = MarketAccount.expect_points_account(tx, market_id)
    pool_accounts = [
        MarketAccount.expect_pool_account(tx, market_id, outcome.id)
        for outcome in outcomes.outcomes
    ]

    # Probabilities are proportional to exp(-pool_balance) per share, for the
    # logarithmic market scoring rule.
    numers_before = [math.exp(-pool_account.balance.amount) for pool_account in pool_accounts]
    denom_before = sum(numers_before)
    ps_before = [x / denom_before for x in numers_before]

    # Normalize the probabilities that the user put in, because nothing else
    # guarantees that they sum to one.
    numers_after = []
    for outcome in outcomes.outcomes:
        v = request.args.get(f"outcome{outcome.id}")
        try:
            numers_after.append(Decimal(v))
        except:
            numers_after.append(Decimal('0.00'))

    denom_after = sum(numers_after)
    ps_after = [x / denom_after for x in numers_after]

    costs: List[Shares] = []
    for outcome, p_before, p_after in zip(outcomes.outcomes, ps_before, ps_after):
        # TODO: Take scaling factor into account.
        reward_diff = Decimal.from_float(math.log(p_after) - math.log(p_before))
        costs.append(Shares(reward_diff, outcome_id=outcome.id))

    # We can't pay negative amounts, if we want to do that, we need to create
    # outcome shares of all the other outcomes instead.
    # TODO: Take current balance into account.
    offset = -min(min([x.amount for x in costs]), Decimal('0.00'))
    for i, outcome in enumerate(outcomes.outcomes):
        costs[i] += Shares(offset, outcome_id=outcome.id)

    total_cost = Points(sum(x.amount for x in costs))

    # What if we can't move the probabilities as much as we want to? Then we
    # need to reduce how much we spend. But that in turn will affect the new
    # probabilities.
    if total_cost > max_spend:
        scale = max_spend.amount / total_cost.amount
        for i, outcome in enumerate(outcomes.outcomes):
            costs[i] = Shares(costs[i].amount * scale, outcome_id=outcome.id)
            reward_diff = float(costs[i].amount - offset * scale)
            p_before = ps_before[i]
            # TODO: Work out the formula to make the probabilities normalized again.
            p_after = math.exp(reward_diff + math.log(p_before))
            ps_after[i] = Decimal.from_float(p_after)

    return Response.ok_html(
        render_template(
            "market_checkout.html",
            session_user=session_user,
            market=market,
            outcomes=outcomes,
            ps_before=ps_before,
            ps_after=ps_after,
            costs=costs,
            zip=zip
        )
    )
