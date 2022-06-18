# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

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

    points_account = UserAccount.expect_points_account(tx, session.user_id)

    return SessionUser(user, points_account.balance)
