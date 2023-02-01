# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from datetime import datetime
from decimal import Decimal
from typing import Iterable, NamedTuple, Optional, Tuple

from hanson.database import Transaction
from hanson.models.currency import Points


class Market(NamedTuple):
    id: int
    author_user_id: int
    title: str
    description: str
    resolution_id: Optional[int]
    created_at: datetime

    @staticmethod
    def create(
        tx: Transaction,
        author_user_id: int,
        title: str,
        description: str,
    ) -> Market:
        market_id, created_at = tx.execute_fetch_one(
            """
            INSERT INTO markets (author_user_id)
            VALUES (%s)
            RETURNING id, created_at;
            """,
            (author_user_id,),
        )
        tx.execute(
            """
            INSERT INTO market_titles (market_id, title) VALUES (%s, %s);
            """,
            (market_id, title),
        )
        tx.execute(
            """
            INSERT INTO market_descriptions (market_id, description) VALUES (%s, %s);
            """,
            (market_id, description),
        )
        resolution_id = None
        return Market(
            market_id, author_user_id, title, description, resolution_id, created_at
        )

    @staticmethod
    def get_by_id(tx: Transaction, market_id: int) -> Optional[Market]:
        result: Optional[
            Tuple[int, int, str, str, Optional[int], datetime]
        ] = tx.execute_fetch_optional(
            """
            SELECT
              id,
              author_user_id,
              current_title,
              current_description,
              current_resolution_id,
              created_at
            FROM
              markets_ext
            WHERE
              id = %s
            """,
            (market_id,),
        )

        if result is None:
            return None

        # We promise the type system that no field is none, but the title
        # and description are not enforced by the database to not be null:
        # there could be no rows (which would be a bug).
        assert result[2] is not None
        assert result[3] is not None

        return Market(*result)

    @staticmethod
    def list_all_with_capitalization(
        tx: Transaction,
    ) -> Iterable[Tuple[Market, Points]]:
        for result in tx.execute_fetch_all(
            """
            SELECT
              id,
              author_user_id,
              current_title,
              current_description,
              current_resolution_id,
              created_at,
              (
                SELECT current_balance
                FROM   accounts_ext
                WHERE  type = 'points' AND owner_market_id = markets_ext.id
              ) as capitalization
            FROM
              markets_ext
            ORDER BY
              capitalization DESC;
            """,
        ):
            # We promise the type system that no field is none, but the title
            # and description are not enforced by the database to not be null:
            # there could be no rows (which would be a bug).
            assert result[2] is not None
            assert result[3] is not None

            capitalization = Points(result[-1])
            yield Market(*result[:-1]), capitalization

    def update_description(self, tx: Transaction, new_description: str) -> Market:
        tx.execute(
            """
            INSERT INTO market_descriptions (market_id, description) VALUES (%s, %s);
            """,
            (self.id, new_description),
        )
        return self._replace(description=new_description)

    def resolve(self, tx: Transaction, resolver_user_id: int) -> None:
        tx.execute(
            """
            INSERT INTO resolutions (market_id, resolver) VALUES (%s, %s);
            """,
            (self.id, resolver_user_id),
        )
        # TODO: Insert current values and mark them as such.
        # TODO: Add test for this.

    def get_trading_volume(
        self,
        tx: Transaction,
        start_time: Optional[datetime],
        end_time: Optional[datetime],
    ) -> Points:
        """
        Return how much points were traded in this market in the given time frame.
        Omit the bounds for the total volume. The lower bound is inclusive, the
        upper bound is exclusive.
        """
        volume: Decimal = tx.execute_fetch_scalar(
            """
            select
              coalesce(sum(amount), 0.00)
            from
              mutations,
              subtransactions,
              transactions,
              accounts
            where
              subtransactions.type in ('exchange_create_shares', 'exchange_destroy_shares')
              and subtransactions.transaction_id = transactions.id
              and mutations.subtransaction_id = subtransactions.id
              and (mutations.credit_account_id = accounts.id or mutations.debit_account_id = accounts.id)
              and accounts.owner_market_id = %(market_id)s
              and (transactions.created_at >= %(start_time)s or %(start_time)s is null)
              and (transactions.created_at <  %(end_time)s   or %(end_time)s   is null);
            """,
            {"market_id": self.id, "start_time": start_time, "end_time": end_time},
        )
        return Points(volume)
