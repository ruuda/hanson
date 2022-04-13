from __future__ import annotations

from typing import Dict, Iterable, List, NamedTuple, Optional, cast
from collections import defaultdict

from hanson.models.currency import Points, Shares
from hanson.database import Transaction
from hanson.models.currency import Amount
from hanson.models.account import UserAccount
from hanson.models.market import Market
from hanson.models.outcome import Outcome


class Entry(NamedTuple):
    name: str
    amount_native: Amount
    max_value: Points
    market_value: Points


class Post(NamedTuple):
    name: str
    href: Optional[str]
    max_value: Points
    market_value: Points
    entries: List[Entry]

    @staticmethod
    def for_points(balance: Points) -> Post:
        """
        Build the post for the user's points balance, not linked to any market.
        """
        return Post(
            name="Points",
            href=None,
            max_value=balance,
            market_value=balance,
            entries=[
                Entry(
                    name="Points",
                    amount_native=balance,
                    max_value=balance,
                    market_value=balance,
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

        entries = []
        for share_balance in balances:
            outcome = market_outcomes[share_balance.outcome_id]
            entries.append(
                Entry(
                    name=outcome.name,
                    amount_native=share_balance,
                    # TODO: Compute the max value.
                    max_value=Points(share_balance.amount),
                    # TODO: Compute the market value.
                    market_value=Points(share_balance.amount),
                )
            )

        entries.sort(key=lambda entry: entry.market_value)

        # If `sum` sums over an empty range, it returns 0, so it may return int.
        # In that case we convert to `Points` with `or`, but Mypy does not know
        # that `sum` returns 0 if it returns an int at all, so cast here.
        total_market_value = cast(
            Points, sum(entry.market_value for entry in entries) or Points.zero()
        )
        max_value: Points = max(*(entry.max_value for entry in entries))

        return Post(
            name=market.title,
            href=f"/markets/{market_id}",
            max_value=max_value,
            market_value=total_market_value,
            entries=entries,
        )


class AssetReport(NamedTuple):
    market_value: Points
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
            max_value=cast(Points, sum(p.max_value for p in posts)),
            posts=posts,
        )
