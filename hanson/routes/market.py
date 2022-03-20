from flask import Blueprint

from hanson.database import Transaction
from hanson.http import Response
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="market", import_name=__name__)


@app.get("/markets")
@with_tx
def route_get_markets(tx: Transaction) -> Response:
    _user = get_session_user(tx)
    return Response.internal_error("TODO: Implement markets page.")


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
    return Response.internal_error("TODO: Implement market page.")
