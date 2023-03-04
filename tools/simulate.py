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


from hanson.database import Transaction
from hanson.models.color import Color
from hanson.models.config import Config
from hanson.models.currency import Points
from hanson.models.outcome import Outcome
from hanson.models.market import Market
from hanson.models.probability import ProbabilityDistribution
from hanson.models.user import User


def add_users(tx: Transaction) -> List[User]:
    return [
        User.create(tx, "etyrell", "Eldon Tyrell"),
        User.create(tx, "lkowalski", "Leon Kowalski"),
        User.create(tx, "rbatty", "Roy Batty"),
        User.create(tx, "rdeckard", "Rick Deckard"),
    ]


def add_markets(tx, author: User) -> List[Market]:
    markets = []

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="Does Deckard know?",
        description="Details omitted to avoid spoilers.",
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
    )
    for i in range(10):
        month = 10 + i
        year = 2019 + (month - 1) // 12
        tt = datetime(year, month, 1, 0, 0, 0, tzinfo=timezone.utc)
        Outcome.create_datetime(
            tx, market.id, f"{year}-{month:02}", Color.from_html_hex("#ff0000"), tt
        )
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
    )
    n = 5
    c0 = Color.from_html_hex("#006cff").to_cieluv()
    c1 = Color.from_html_hex("#ff8100").to_cieluv()
    for i in range(n):
        # Values are exponentially increasing.
        v: int = 10 * (2 ** i)
        # The colors are interpolated between the endpoints, in CIELUV space.
        t: float = i / (n - 1)
        c = Color.from_cieluv(*(c0x * (1.0 - t) + c1x * t for c0x, c1x in zip(c0, c1)))
        Outcome.create_float(tx, market.id, str(v), c, float(v))
    markets.append(market)

    return markets
