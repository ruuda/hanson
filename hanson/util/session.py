from __future__ import annotations

from hanson.database import Transaction
from hanson.models.session import Session
from hanson.models.user import User


class NotLoggedInError(Exception):
    pass


def get_session_user(tx: Transaction) -> User:
    """
    Returns the user that is currently logged in.
    If no user is logged in, raises a NotLoggedIn exception,
    which is handled at the application level to redirect to /login.
    """
    session = Session.get_from_cookie(tx)
    if session is None:
        raise NotLoggedInError()

    user = User.get_by_id(tx, session.user_id)
    assert user is not None
    return user
