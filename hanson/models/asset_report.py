# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import Dict, Iterable, List, NamedTuple, Optional, cast
from collections import defaultdict
from decimal import Decimal

from hanson.models.currency import Points, Shares
from hanson.database import Transaction
from hanson.models.currency import Amount
from hanson.models.account import MarketAccount, UserAccount
from hanson.models.market import Market
from hanson.models.outcome import Outcome
from hanson.models.probability import ProbabilityDistribution


class Entry(NamedTuple):
    name: str
    amount_native: Amount
    market_value: Points
    most_likely_value: Points
    max_value: Points


class Post(NamedTuple):
    name: str
    href: Optional[str]
    market_value: Points
    most_likely_value: Points
    max_value: Points
    entries: List[Entry]

    @staticmethod
    def for_points(balance: Points) -> Post:
        """
        Build the post for the user's points balance, not linked to any market.
        """
        return Post(
            name="Points",
            href=None,
            market_value=balance,
            most_likely_value=balance,
            max_value=balance,
            entries=[
                Entry(
                    name="Points",
                    amount_native=balance,
                    market_value=balance,
                    most_likely_value=balance,
                    max_value=balance,
                )
            ],
        )

    @staticmethod
    def for_market(tx: Transaction, market_id: int, balances: List[Shares]) -> Post:
        """
        Build the post for a particular market, that includes as entries all of
        the outcomes for which the user has a non-zero balance.
        """
        market = Market.get_by_id(tx, market_id)
        assert market is not None

        market_outcomes = {
            oc.id: oc for oc in Outcome.get_all_by_market(tx, market_id).outcomes
        }

        pool_balances = [
            MarketAccount.expect_pool_account(tx, market_id, outcome_id).balance
            for outcome_id in market_outcomes.keys()
        ]
        pd = ProbabilityDistribution.from_pool_balances(pool_balances)
        max_p_index = pd.most_likely_index()
        max_p_value = Points.zero()

        entries = []
        for i, (share_balance, p) in enumerate(zip(balances, pd.ps())):
            assert share_balance.outcome_id == pool_balances[i].outcome_id
            outcome = market_outcomes[share_balance.outcome_id]

            if share_balance.is_zero():
                continue

            if i == max_p_index:
                # TODO: Take exchange rate into account.
                max_p_value = Points(share_balance.amount)

            entries.append(
                Entry(
                    name=outcome.name,
                    amount_native=share_balance,
                    # TODO: Take exchange rate into account.
                    market_value=Points(share_balance.amount * Decimal(p)),
                    most_likely_value=max_p_value if i == max_p_index else Points.zero(),
                    # TODO: Take exchange rate into account.
                    max_value=Points(share_balance.amount),
                )
            )

        entries.sort(key=lambda entry: entry.market_value, reverse=True)

        # If `sum` sums over an empty range, it returns 0, so it may return int.
        # In that case we convert to `Points` with `or`, but Mypy does not know
        # that `sum` returns 0 if it returns an int at all, so cast here.
        total_market_value = cast(
            Points, sum(entry.market_value for entry in entries) or Points.zero()
        )
        max_value: Points = max(entry.max_value for entry in entries)

        return Post(
            name=market.title,
            href=f"/market/{market_id}",
            market_value=total_market_value,
            most_likely_value=max_p_value,
            max_value=max_value,
            entries=entries,
        )


class AssetReport(NamedTuple):
    market_value: Points
    most_likely_value: Points
    max_value: Points
    posts: List[Post]

    @staticmethod
    def get_for_user(tx: Transaction, user_id: int) -> AssetReport:
        markets: Dict[int, List[Shares]] = defaultdict(lambda: [])
        user_points_balance: Points = Points.zero()

        # Gather the balance of outcome shares per market.
        for account in UserAccount.list_all_for_user(tx, user_id):
            if isinstance(account.balance, Shares):
                assert account.market_id is not None
                markets[account.market_id].append(account.balance)

            elif isinstance(account.balance, Points):
                assert account.market_id is None
                user_points_balance = account.balance

            else:
                raise Exception("Invalid account balance type.")

        posts = [
            Post.for_points(user_points_balance),
            *(
                Post.for_market(tx, market_id, balances)
                for market_id, balances in markets.items()
            ),
        ]

        # The `sum`s below sum over a non-empty list, and therefore they do not
        # return `0: int`, they return `Points`. But Mypy does not know that, so
        # use a cast here.
        return AssetReport(
            market_value=cast(Points, sum(p.market_value for p in posts)),
            most_likely_value=cast(Points, sum(p.most_likely_value for p in posts)),
            max_value=cast(Points, sum(p.max_value for p in posts)),
            posts=posts,
        )
