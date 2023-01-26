# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from datetime import datetime
from typing import Iterable, NamedTuple, Optional, Tuple

from hanson.database import Transaction
from hanson.models.currency import Points


class Market(NamedTuple):
    id: int
    author_user_id: int
    title: str
    description: str
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
            INSERT INTO "market" (author_user_id)
            VALUES (%s)
            RETURNING id, created_at;
            """,
            (author_user_id,),
        )
        tx.execute(
            """
            INSERT INTO "market_title" (market_id, title) VALUES (%s, %s);
            """,
            (market_id, title),
        )
        tx.execute(
            """
            INSERT INTO "market_description" (market_id, description) VALUES (%s, %s);
            """,
            (market_id, description),
        )
        return Market(market_id, author_user_id, title, description, created_at)

    @staticmethod
    def get_by_id(tx: Transaction, market_id: int) -> Optional[Market]:
        result: Optional[
            Tuple[int, int, str, str, datetime]
        ] = tx.execute_fetch_optional(
            """
            SELECT
              id,
              author_user_id,
              market_current_title(id),
              market_current_description(id),
              created_at
            FROM
              "market"
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
        assert all(field is not None for field in result)

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
              market_current_title(id),
              market_current_description(id),
              created_at,
              (
                SELECT COALESCE(account_current_balance(account.id), 0.00)
                FROM   account
                WHERE  type = 'points' AND owner_market_id = market.id
              ) as capitalization
            FROM
              "market"
            ORDER BY
              capitalization DESC;
            """,
        ):
            # We promise the type system that no field is none, but the title
            # and description are not enforced by the database to not be null:
            # there could be no rows (which would be a bug).
            assert all(field is not None for field in result)
            capitalization = Points(result[-1])
            yield Market(*result[:-1]), capitalization

    def update_description(self, tx: Transaction, new_description: str) -> Market:
        tx.execute(
            """
            INSERT INTO "market_description" (market_id, description) VALUES (%s, %s);
            """,
            (self.id, new_description),
        )
        return self._replace(description=new_description)
