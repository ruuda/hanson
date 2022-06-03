from __future__ import annotations

from datetime import datetime, timedelta
from decimal import Decimal
from typing import Dict, List, NamedTuple, Tuple, Iterable, cast

from hanson.database import Transaction
from hanson.models.currency import Shares
from hanson.models.probability import ProbabilityDistribution


def fill_all_balances(
    updates: Iterable[Tuple[datetime, int, Decimal]],
) -> Iterable[Tuple[datetime, List[Shares]]]:
    """
    Given updates of the form (time, outcome_id, new_balance) that are ordered
    by time and then by outcome id, return the balances for every unique time.
    This holds the last known balance for an account if its balance was not
    updated at some time. The first timestamp must have an update for every
    account id.
    """
    balances: Dict[int, Shares] = {}
    current_time = None

    for time, outcome_id, new_balance in updates:
        if current_time is None:
            # This is the first update.
            current_time = time

        elif current_time < time:
            # We started a new timestamp, the data for the previous timestamp
            # is now complete.
            yield current_time, list(balances.values())
            current_time = time

        elif current_time == time:
            # Just another element for this timestamp.
            pass

        else:
            raise Exception("Timestamps are not in ascending order.")

        balances[outcome_id] = Shares(amount=new_balance, outcome_id=outcome_id)

    # Don't forget the final bucket.
    if current_time is not None:
        yield current_time, list(balances.values())


class ProbabilityHistory(NamedTuple):
    """
    The probability distribution of a market at different points in time.

    For the history, we divide time into bins. The time in the history list is
    the end of the bin interval, and the probability distribution is the one at
    the end of the bin time.
    """
    bin_size: timedelta
    history: List[Tuple[datetime, ProbabilityDistribution]]

    @staticmethod
    def from_balance_updates(
        bin_size: timedelta,
        updates: Iterable[Tuple[datetime, int, Decimal]],
    ) -> ProbabilityHistory:
        """
        Return probability distributions at the unique times part of updates.
        See also `fill_all_balances`. The elements of the probability
        distribution are ordered by ascending outcome id.
        """
        return ProbabilityHistory(
            bin_size,
            [
                (time, ProbabilityDistribution.from_pool_balances(balances))
                for time, balances
                in fill_all_balances(updates)
            ],
        )

    @staticmethod
    def for_market(
        tx: Transaction,
        *,
        market_id: int,
        bin_size: timedelta,
    ) -> ProbabilityHistory:
        """
        Return the historical probabilities for a given market, sampled at
        regular intervals.
        """
        updates = tx.execute_fetch_all(
            """
            select distinct on (
                bin_end,
                outcome_id
              )
              -- date_bin rounds down, so we would group on the start time of
              -- the bin, but we get the balance at the end of the bin, so we
              -- should also return the end time of the bin.
              date_bin(%(bin_size)s, created_at, '2022-01-01T00:00:00+00:00') + %(bin_size)s as bin_end,
              outcome_id,
              post_balance as bin_end_balance
            from
              account_balance,
              account,
              mutation,
              subtransaction,
              transaction
            where
              account_balance.mutation_id = mutation.id
              and account_balance.account_id = account.id
              and mutation.subtransaction_id = subtransaction.id
              and subtransaction.transaction_id = transaction.id
              and account.owner_market_id = %(market_id)s
              and account.type = 'shares'
            order by
              bin_end,
              outcome_id,
              -- Per bin, we want the balance of the latest mutation in that bin.
              mutation.id desc;
            """,
            {
                "bin_size": bin_size,
                "market_id": market_id,
            },
        )
        updates_typed = cast(Iterable[Tuple[datetime, int, Decimal]], updates)
        return ProbabilityHistory.from_balance_updates(bin_size, updates_typed)
