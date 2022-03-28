from datetime import datetime, timezone

from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.models.session import Session
from hanson.models.user import User
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user

app = Blueprint(name="session", import_name=__name__)


@app.get("/login")
def route_get_login() -> Response:
    return Response.ok_html(render_template("login.html"))


@app.post("/login")
@with_tx
def route_post_login(tx: Transaction) -> Response:
    from flask import request

    if "username" not in request.form:
        return Response.bad_request("Expected 'username' parameter.")

    # For development, we just log the user in by name without any authentication.
    # For production, at some point I should add OAuth support.
    user = User.get_by_username(tx, request.form["username"])
    if user is None:
        return Response.bad_request("No such user.")

    session = Session.create(tx, user.id)
    tx.commit()

    response = Response.redirect_see_other("/")
    response.add_set_cookie_header("session", str(session.token), session.expires_at)
    return response


@app.get("/logout")
@with_tx
def route_get_logout(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    # The template just shows a button to make the POST request.
    return Response.ok_html(
        render_template(
            "logout.html",
            session_user=session_user,
        )
    )


@app.post("/logout")
def route_post_logout() -> Response:
    response = Response.redirect_see_other("/")
    # Clear and expire the session cookie.
    date_in_past = datetime(2000, 1, 1, 0, 0, 0, tzinfo=timezone.utc)
    response.add_set_cookie_header("session", "", date_in_past)
    # TODO: We should also invalidate the session in the database.
    return response
