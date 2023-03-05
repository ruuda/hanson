#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
The simulator simulates users trading, filling the database with dummy data.
"""

from typing import List
from datetime import datetime, timezone
from decimal import Decimal

from hanson.database import Transaction, connect_config
from hanson.models.color import Color
from hanson.models.config import Config
from hanson.models.currency import Points
from hanson.models.outcome import Outcome
from hanson.models.market import Market
from hanson.models.probability import ProbabilityDistribution
from hanson.models.user import User
from hanson.models.transaction import (
    create_transaction_income,
    create_transaction_fund_market,
)


def add_users(tx: Transaction) -> List[User]:
    return [
        User.create(tx, "etyrell", "Eldon Tyrell"),
        User.create(tx, "lkowalski", "Leon Kowalski"),
        User.create(tx, "rbatty", "Roy Batty"),
        User.create(tx, "rdeckard", "Rick Deckard"),
    ]


def add_markets(tx: Transaction, author: User, now: datetime) -> List[Market]:
    assert now.tzinfo is not None
    markets = []

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="Does Deckard know?",
        description="Details omitted to avoid spoilers.",
        now=now,
    )
    Outcome.create_discrete(tx, market.id, "Yes", Color.from_html_hex("#68a223"))
    Outcome.create_discrete(tx, market.id, "No", Color.from_html_hex("#a22a17"))
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="Who will be retired first?",
        description=(
            "Deckard has been tasked with retiring four replicants. "
            "Who will he retire first?"
        ),
        now=now,
    )
    Outcome.create_discrete(tx, market.id, "Leon", Color.from_html_hex("#53a5ba"))
    Outcome.create_discrete(tx, market.id, "Pris", Color.from_html_hex("#c0321b"))
    Outcome.create_discrete(tx, market.id, "Roy", Color.from_html_hex("#fff66e"))
    Outcome.create_discrete(tx, market.id, "Zhora", Color.from_html_hex("#8153ba"))
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="When will Roy stop functioning?",
        description=(
            "Roy was activated on January 8, 2016. "
            "But as Tyrell put it, the light that burns twice as bright "
            "burns half as long — and Roy has burned so very, very brightly."
        ),
        now=now,
    )
    n = 10
    c0 = Color.from_html_hex("#006cff")
    c1 = Color.from_html_hex("#ff8100")
    for i in range(n):
        month = ((9 + i) % 12) + 1
        year = 2019 + (9 + i) // 12
        # The colors are interpolated between the endpoints, in CIELUV space.
        c = Color.interpolate_cieluv(c0, c1, i / (n - 1))
        tt = datetime(year, month, 1, 0, 0, 0, tzinfo=timezone.utc)
        Outcome.create_datetime(tx, market.id, f"{year}-{month:02}", c, tt)
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="How many questions will be needed to determine Rachael’s nature?",
        description=(
            "The Voight-Kampff test is a test used to assess whether individuals "
            "are human or replicants by asking them questions and measuring "
            "their physiological response. Typically it takes twenty to thirty "
            "cross-referenced questions to detect a Nexus-6 replicant. "
            "How many questions will Rachael need to answer before the "
            "Voight-Kampff test is conclusive?"
        ),
        now=now,
    )
    n = 5
    for i in range(n):
        # Values are exponentially increasing.
        v: int = 10 * (2**i)
        # The colors are interpolated between the endpoints, in CIELUV space.
        c = Color.interpolate_cieluv(c0, c1, i / (n - 1))
        Outcome.create_float(tx, market.id, str(v), c, float(v))
    markets.append(market)

    return markets


def add_income(
    tx: Transaction, users: List[User], amount: Points, now: datetime
) -> None:
    for user in users:
        create_transaction_income(tx, user.id, amount, now)


def main() -> None:
    # Backdate market creation to a fixed time in the past, so we can run
    # the simulation for a ~year and also see the graphs in the webinterface.
    now = datetime(2022, 3, 1, 15, 0, 0, tzinfo=timezone.utc)

    config = Config.load_from_toml_file("config.toml")
    conn = connect_config(config)
    with conn.begin() as tx:
        users = add_users(tx)

        add_income(tx, users, Points(Decimal("10.00")), now)

        # For simplicity, the first user is going to be the author of all markets.
        etyrell = users[0]
        markets = add_markets(tx, etyrell, now)

        # We give this user some additional income to pay for funding the markets.
        create_transaction_income(
            tx, etyrell.id, Points(Decimal("5.0") * len(markets)), now
        )
        for market in markets:
            create_transaction_fund_market(
                tx, etyrell.id, market.id, Points(Decimal("5.00")), now
            )

        tx.commit()


if __name__ == "__main__":
    main()
