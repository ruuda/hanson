from __future__ import  annotations

from flask import Flask, request, make_response, render_template
from typing import Dict, NamedTuple
from datetime import datetime

from hanson.database import connect_default

app = Flask("hanson")
dbconn = connect_default()


class Response(NamedTuple):
    body: str
    status_code: int
    headers: Dict[str, str]

    @staticmethod
    def ok_html(html: str) -> Response:
        return Response(
            body=html,
            status_code=200,
            headers={
                "Content-Type": "text/html; charset=utf-8",
            }
        )

    @staticmethod
    def bad_request(message: str) -> Response:
        return Response(
            body=message,
            status_code=400,
            headers={
                "Content-Type": "text/plain; charset=utf-8",
            }
        )

    @staticmethod
    def redirect_see_other(location: str) -> Response:
        """
        Send a 303 See Other. Can be used from a POST, and the browser will do a GET.
        """
        return Response(
            body="",
            status_code=303,
            headers={
                "Location": location,
            }
        )

    def add_set_cookie_header(
        self,
        cookie_name: str,
        cookie_value: str,
        expires: datetime
    ) -> None:
        from email.utils import format_datetime
        assert expires.tzinfo is not None

        self.headers["Set-Cookie"] = (
            f"{cookie_name}={cookie_value}; "
            f"Expires={format_datetime(expires, usegmt=True)}; "
            "HttpOnly; "
            "SameSite=Strict"
            # TODO: Enable "secure" for production deployments;
            # we cannot enable it locally.
        )


@app.route("/")
@app.route("/~<name>")
def hello_world(name: str = "visitor") -> Response:
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
