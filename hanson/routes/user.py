from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.market import Market
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="user", import_name=__name__)


@app.get("/users")
@with_tx
def route_get_users(tx: Transaction) -> Response:
    _session_user = get_session_user(tx)
    return Response.internal_error("TODO: Implement this.")


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
            "user.html",
            session_user=session_user,
            profile_user=profile_user,
        )
    )
