#!/usr/bin/env python3

from hanson.database import Transaction, connect_default
from hanson.models.user import User
from hanson.models.session import Session
from hanson.models.market import Market
from hanson.models.outcome import Outcome

from datetime import datetime, timezone
from typing import List
from textwrap import dedent


def add_users(tx: Transaction) -> List[User]:
    henk = User.create(tx, "henk", "Henk de Steen")
    piet = User.create(tx, "piet", "Piet de Kei")
    return [henk, piet]


def add_sessions(tx: Transaction, users: List[User]) -> List[Session]:
    return [Session.create(tx, user.id) for user in users]


def add_markets(tx: Transaction, users: List[User]) -> List[Market]:
    markets = []
    for user in users:
        market = Market.create(
            tx,
            author_user_id=user.id,
            title=f"Will {user.username.title()} frobnicate the widget this year?",
            description=dedent(
                f"""
                In past years, {user.username.title()} has shown a tendency to
                frobnicate widgets, although in some years, no widget was
                frobnicated. Will {user.username.title()} frobnicate at least one
                widget this year?

                ## Resolution criteria

                Resolves to _yes_ when {user.username.title()} frobnicates at
                least one widget this year, or to _no_ otherwise.
                """,
            ),
        )
        Outcome.create_discrete(tx, market.id, "Yes", "#68a223")
        Outcome.create_discrete(tx, market.id, "No", "#a22a17")
        markets.append(market)

        market = Market.create(
            tx,
            author_user_id=user.id,
            title=f"What will {user.username.title()} have for lunch?",
            description=dedent(
                f"""
                {user.username.title()} is known to have bread with peanut
                butter for lunch *every single day*. But perhaps today will be
                different?
                """,
            ),
        )
        Outcome.create_discrete(tx, market.id, "Peanut butter", "#b58800")
        Outcome.create_discrete(tx, market.id, "Cheese", "#d2bb00")
        Outcome.create_discrete(tx, market.id, "Ham", "#d58b8b")
        markets.append(market)

        market = Market.create(
            tx,
            author_user_id=user.id,
            title=f"When will {user.username.title()} graduate?",
            description=dedent(
                f"""
                {user.username.title()} has been studying for four year already,
                but still hasn’t passed the freshman course “Linear Algebra I”.
                Will {user.username.title()} complete it this year?
                """,
            ),
        )
        # TODO: Should all outcomes have a name? Maybe float and datetime outcomes
        # should be named by the system instead.
        t = datetime(2022, 6, 1, 0, 0, 0, tzinfo=timezone.utc)
        Outcome.create_datetime(tx, market.id, "2022-06", "#ff0000", t)
        Outcome.create_datetime(
            tx, market.id, "2023-06", "#880088", t.replace(year=2023)
        )
        Outcome.create_datetime(
            tx, market.id, "2024-06", "#0000ff", t.replace(year=2024)
        )
        markets.append(market)

        market = Market.create(
            tx,
            author_user_id=user.id,
            title=f"How many liters of milk can {user.username.title()} drink?",
            description=dedent(
                f"""
                We are going to hold a milk drinking contest. For two hours,
                participants can drink as much milk as they can. How many liters
                of milk will {user.username.title()} consume during this
                contest?
                """,
            ),
        )
        Outcome.create_float(tx, market.id, "0.2", "#ff0000", 0.2)
        Outcome.create_float(tx, market.id, "0.5", "#aa0022", 0.5)
        Outcome.create_float(tx, market.id, "1.0", "#2200aa", 1.0)
        Outcome.create_float(tx, market.id, "2.0", "#0000ff", 2.0)
        markets.append(market)

    return markets


def main() -> None:
    conn = connect_default()
    with conn.begin() as tx:
        users = add_users(tx)
        _sessions = add_sessions(tx, users)
        _markets = add_markets(tx, users)
        tx.commit()


if __name__ == "__main__":
    main()
