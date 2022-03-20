from flask import Blueprint, render_template

from hanson import database as db
from hanson.http import Response
from hanson.models.session import Session
from hanson.models.user import User


app = Blueprint(name="index", import_name=__name__)


@app.route("/")
def route_get_index() -> Response:
    with db.get_context_connection().begin() as tx:
        session = Session.get_from_cookie(tx)
        if session is None:
            return Response.redirect_see_other("/login")

        user = User.get_by_id(tx, session.user_id)
        assert user is not None
        return Response.ok_html(render_template("index.html", name=user.name))


@app.route("/~<name>")
def hello_world(name: str) -> Response:
    return Response.ok_html(render_template("index.html", name=name))
