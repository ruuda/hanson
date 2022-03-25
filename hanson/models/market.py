from __future__ import annotations

from typing import NamedTuple, Optional, Tuple
from datetime import datetime
from uuid import UUID, uuid4

from hanson.database import Transaction


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
        with tx.cursor() as cur:
            cur.execute(
                """
                INSERT INTO "market" (author_user_id)
                VALUES (%s)
                RETURNING id, created_at;
                """,
                (author_user_id,),
            )
            market_id, created_at = cur.fetchone()
            cur.execute(
                """
                INSERT INTO "market_title" (market_id, title) VALUES (%s, %s);
                """,
                (market_id, title),
            )
            cur.execute(
                """
                INSERT INTO "market_description" (market_id, description) VALUES (%s, %s);
                """,
                (market_id, description),
            )
            return Market(market_id, author_user_id, title, description, created_at)

    @staticmethod
    def get_by_id(tx: Transaction, market_id: int) -> Optional[Market]:
        with tx.cursor() as cur:
            cur.execute(
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
            result: Optional[Tuple[int, int, str, str, datetime]] = cur.fetchone()

            if result is None:
                return None

            # We promise the type system that no field is none, but the title
            # and description are not enforced by the database to not be null:
            # there could be no rows (which would be a bug).
            assert all(field is not None for field in result)

            return Market(*result)
