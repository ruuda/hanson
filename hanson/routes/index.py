from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="index", import_name=__name__)


@app.get("/")
@with_tx
def route_get_index(tx: Transaction) -> Response:
    user = get_session_user(tx)
    return Response.ok_html(render_template("index.html", name=user.name))


@app.get("/~<name>")
def hello_world(name: str) -> Response:
    return Response.ok_html(render_template("index.html", name=name))
