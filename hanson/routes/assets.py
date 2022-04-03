from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="assets", import_name=__name__)


@app.get("/assets")
@with_tx
def route_get_assets(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    return Response.ok_html(
        render_template(
            "assets.html",
            session_user=session_user,
            market_assets=[],
        )
    )
