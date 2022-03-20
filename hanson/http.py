from __future__ import annotations

from datetime import datetime
from typing import Dict, NamedTuple


class Response(NamedTuple):
    """
    A response type compatible with Flask's (body, status code, headers) tuple.
    """

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
            },
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
            },
        )

    @staticmethod
    def bad_request(message: str) -> Response:
        return Response(
            body=message,
            status_code=400,
            headers={
                "Content-Type": "text/plain; charset=utf-8",
            },
        )

    @staticmethod
    def internal_error(message: str) -> Response:
        return Response(
            body=message,
            status_code=500,
            headers={
                "Content-Type": "text/plain; charset=utf-8",
            },
        )

    def add_set_cookie_header(
        self, cookie_name: str, cookie_value: str, expires: datetime
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
