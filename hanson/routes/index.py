from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="index", import_name=__name__)


@app.get("/")
@with_tx
def route_get_index(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    return Response.ok_html(
        render_template(
            "index.html",
            name=session_user.username,
            session_user=session_user,
        )
    )


@app.get("/~<username>")
def route_user_home(username: str) -> Response:
    return Response.redirect_moved_permanently(f"/user/{username}")


@app.get("/user/<username>")
@with_tx
def hello_world(tx: Transaction, username: str) -> Response:
    session_user = get_session_user(tx)
    profile_user = User.get_by_username(tx, username)

    if profile_user is None:
        return Response.not_found("No such user.")

    return Response.ok_html(
        render_template(
            "index.html",
            name=username,
            session_user=session_user,
        )
    )
