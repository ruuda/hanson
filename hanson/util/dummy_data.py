#!/usr/bin/env python3

from hanson.database import Transaction, connect_default
from hanson.models.user import User
from hanson.models.session import Session
from hanson.models.market import Market

from typing import List
from textwrap import dedent


def add_users(tx: Transaction) -> List[User]:
    henk = User.create(tx, "henk")
    piet = User.create(tx, "piet")
    return [henk, piet]


def add_sessions(tx: Transaction, users: List[User]) -> List[Session]:
    return [Session.create(tx, user.id) for user in users]


def add_markets(tx: Transaction, users: List[User]) -> List[Market]:
    return [
        Market.create(
            tx,
            author_user_id=user.id,
            title=f"Will {user.name.title()} frobnicate the widget this year?",
            description=dedent(
                f"""
                In past years, {user.name.title()} has shown a tendency to
                frobnicate widgets, although in some years, no widget was
                frobnicated. Will {user.name.title()} frobnicate at least one
                widget this year?

                ## Resolution criteria

                Resolves to _yes_ when {user.name.title()} frobnicates at least
                one widget this year, or to _no_ otherwise.
                """,
            )
        )
        for user in users
    ]


def main() -> None:
    conn = connect_default()
    with conn.begin() as tx:
        users = add_users(tx)
        _sessions = add_sessions(tx, users)
        _markets = add_markets(tx, users)
        tx.commit()


if __name__ == "__main__":
    main()
