#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from datetime import datetime, timezone
from decimal import Decimal
from textwrap import dedent
from typing import List

from hanson.database import Transaction, connect_default
from hanson.models.currency import Points
from hanson.models.market import Market
from hanson.models.outcome import Outcome
from hanson.models.color import Color
from hanson.models.session import Session
from hanson.models.transaction import (
    create_transaction_income,
    create_transaction_fund_market,
)
from hanson.models.user import User


def add_users(tx: Transaction) -> List[User]:
    henk = User.create(tx, "henk", "Henk de Steen")
    piet = User.create(tx, "piet", "Piet de Kei")
    return [henk, piet]


def add_sessions(tx: Transaction, users: List[User]) -> List[Session]:
    return [Session.create(tx, user.id) for user in users]


def add_income(tx: Transaction, users: List[User], amount: Points) -> None:
    for user in users:
        create_transaction_income(tx, user.id, amount)


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
        Outcome.create_discrete(tx, market.id, "Yes", Color.from_html_hex("#68a223"))
        Outcome.create_discrete(tx, market.id, "No", Color.from_html_hex("#a22a17"))
        create_transaction_fund_market(tx, user.id, market.id, Points(Decimal("2.00")))
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
        Outcome.create_discrete(
            tx, market.id, "Peanut butter", Color.from_html_hex("#b58800")
        )
        Outcome.create_discrete(tx, market.id, "Cheese", Color.from_html_hex("#d2bb00"))
        Outcome.create_discrete(tx, market.id, "Ham", Color.from_html_hex("#d58b8b"))
        create_transaction_fund_market(tx, user.id, market.id, Points(Decimal("2.00")))
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
        Outcome.create_datetime(
            tx, market.id, "2022-06", Color.from_html_hex("#ff0000"), t
        )
        Outcome.create_datetime(
            tx,
            market.id,
            "2023-06",
            Color.from_html_hex("#880088"),
            t.replace(year=2023),
        )
        Outcome.create_datetime(
            tx,
            market.id,
            "2024-06",
            Color.from_html_hex("#0000ff"),
            t.replace(year=2024),
        )
        create_transaction_fund_market(tx, user.id, market.id, Points(Decimal("2.00")))
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
        Outcome.create_float(tx, market.id, "0.2", Color.from_html_hex("#ff0000"), 0.2)
        Outcome.create_float(tx, market.id, "0.5", Color.from_html_hex("#aa0022"), 0.5)
        Outcome.create_float(tx, market.id, "1.0", Color.from_html_hex("#2200aa"), 1.0)
        Outcome.create_float(tx, market.id, "2.0", Color.from_html_hex("#0000ff"), 2.0)
        create_transaction_fund_market(tx, user.id, market.id, Points(Decimal("2.00")))
        markets.append(market)

    return markets


def main() -> None:
    conn = connect_default()
    with conn.begin() as tx:
        users = add_users(tx)

        # Give users 10 points initially, but in 10 transactions, so we have
        # something interesting to show in the transaction view.
        for _ in range(10):
            add_income(tx, users, Points(Decimal("1.00")))

        _sessions = add_sessions(tx, users)
        _markets = add_markets(tx, users)

        tx.commit()


if __name__ == "__main__":
    main()
