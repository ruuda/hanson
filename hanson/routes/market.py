from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.market import Market
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="market", import_name=__name__)


@app.get("/markets")
@with_tx
def route_get_markets(tx: Transaction) -> Response:
    _user = get_session_user(tx)
    markets = Market.list_all(tx)
    return Response.ok_html(
        render_template("market_index.html", markets=markets)
    )


@app.get("/market/new")
@with_tx
def route_get_market_new(tx: Transaction) -> Response:
    _user = get_session_user(tx)
    return Response.internal_error("TODO: Implement new market page.")


@app.post("/market/new")
@with_tx
def route_post_market_new(tx: Transaction) -> Response:
    _user = get_session_user(tx)
    return Response.internal_error("TODO: Implement post new market page.")


@app.get("/market/<int:market_id>")
@with_tx
def route_get_market(tx: Transaction, market_id: int) -> Response:
    _user = get_session_user(tx)

    market = Market.get_by_id(tx, market_id)
    if market is None:
        return Response.not_found("This market does not exist.")

    author = User.get_by_id(tx, market.author_user_id)
    assert author is not None

    return Response.ok_html(
        render_template(
            "market.html",
            market=market,
            author=author,
        )
    )
