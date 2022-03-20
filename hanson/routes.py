from __future__ import  annotations

from flask import Flask, request, make_response, render_template
from datetime import datetime

from hanson.database import connect_default
from hanson.http import Response
from hanson.models.session import Session
from hanson.models.user import User

app = Flask("hanson")
dbconn = connect_default()


@app.route("/")
def route_get_index() -> Response:
    with dbconn.begin() as tx:
        session = Session.get_from_cookie(tx)
        if session is None:
            return Response.redirect_see_other("/login")

        user = User.get_by_id(tx, session.user_id)
        assert user is not None
        return Response.ok_html(render_template("index.html", name=user.name))


@app.route("/~<name>")
def hello_world(name: str) -> Response:
    return Response.ok_html(render_template("index.html", name=name))


@app.get("/login")
def route_get_login() -> Response:
    return Response.ok_html(render_template("login.html"))


@app.post("/login")
def route_post_login() -> Response:
    from hanson.models.user import User
    from hanson.models.session import Session

    if "username" not in request.form:
        return Response.bad_request("Expected 'username' parameter.")

    with dbconn.begin() as tx:
        # For development, we just log the user in by name without any authentication.
        # For production, at some point I should add OAuth support.
        user = User.get_by_name(tx, request.form["username"])
        if user is None:
            return Response.bad_request("No such user.")

        session = Session.create(tx, user.id)
        tx.commit()

    response = Response.redirect_see_other("/")
    response.add_set_cookie_header("session", str(session.token), session.expires_at)
    return response


@app.get("/logout")
def route_get_logout() -> Response:
    # The template just shows a button to make the POST request.
    return Response.ok_html(render_template("logout.html"))


@app.post("/logout")
def route_post_logout() -> Response:
    from datetime import timezone
    response = Response.redirect_see_other("/")
    # Clear and expire the session cookie.
    date_in_past = datetime(2000, 1, 1, 0, 0, 0, tzinfo=timezone.utc)
    response.add_set_cookie_header("session", "", date_in_past)
    # TODO: We should also invalidate the session in the database.
    return response
