from __future__ import annotations

from typing import NamedTuple

from hanson.database import Transaction
from hanson.models.session import Session
from hanson.models.user import User
from hanson.models.currency import Points
from hanson.models.account import UserAccount


class NotLoggedInError(Exception):
    pass


class SessionUser(NamedTuple):
    user: User
    points_balance: Points


def get_session_user(tx: Transaction) -> SessionUser:
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

    points_account = UserAccount.get_points_account(tx, session.user_id)

    return SessionUser(user, points_account.balance)
