from __future__ import annotations

import re

from typing import NamedTuple, Optional, Tuple
from datetime import datetime

from hanson.database import Transaction


class User(NamedTuple):
    id: int
    name: str
    created_at: datetime

    @staticmethod
    def is_valid_username(username: str) -> bool:
        return re.fullmatch("[a-z]+", username) is not None

    @staticmethod
    def create(tx: Transaction, username: str) -> User:
        """
        Create a new user.
        """
        assert User.is_valid_username(username)
        with tx.cursor() as cur:
            cur.execute(
                'INSERT INTO "user" DEFAULT VALUES RETURNING id, created_at;',
            )
            user_id, created_at = cur.fetchone()
            cur.execute(
                "INSERT INTO user_name (user_id, name) VALUES (%s, %s);",
                (user_id, username),
            )
            return User(user_id, username, created_at)

    @staticmethod
    def get_by_name(tx: Transaction, username: str) -> Optional[User]:
        with tx.cursor() as cur:
            cur.execute(
                """
                SELECT user_id
                FROM user_name
                WHERE name = %s
                """,
                (username,),
            )
            result: Optional[Tuple[int]] = cur.fetchone()
            if result is None:
                return None
            else:
                return User.get_by_id(tx, result[0])

    @staticmethod
    def get_by_id(tx: Transaction, user_id: int) -> Optional[User]:
        with tx.cursor() as cur:
            cur.execute(
                """
                SELECT id, user_current_name(id), created_at
                FROM "user"
                WHERE id = %s;
                """,
                (user_id,),
            )
            result: Optional[Tuple[int, str, datetime]] = cur.fetchone()
            if result is None:
                return None
            else:
                return User(*result)
