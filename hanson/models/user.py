# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

import re
from datetime import datetime
from decimal import Decimal
from typing import Iterable, NamedTuple, Optional, Tuple

from hanson.database import Transaction
from hanson.models import account
from hanson.models.currency import Points


class User(NamedTuple):
    id: int
    username: str
    full_name: str
    created_at: datetime

    @staticmethod
    def is_valid_username(username: str) -> bool:
        return re.fullmatch("[a-z]+", username) is not None

    @staticmethod
    def is_valid_full_name(full_name: str) -> bool:
        return (
            len(full_name) > 0
            and User.validate_full_name_get_violation(full_name) is None
        )

    @staticmethod
    def validate_full_name_get_violation(full_name: str) -> Optional[str]:
        """
        Check if a full name is valid. Returns None if it is, or the character
        that makes the name invalid, if it is not. We try to strike a balance
        here between not assuming too much about names and allowing names from
        different cultures, while disallowing emoji and unicode abuse.
        """
        import unicodedata

        valid_categories = {
            "Lu",  # Letter uppercase
            "Ll",  # Letter lowercase
            "Lt",  # Letter titlecase
            "Lm",  # Letter modifier
            "Lo",  # Letter other
            "Mn",  # Mark nonspacing
            "Mc",  # Mark combining
            "Me",  # Mark enclosing
            "Pc",  # Punctucation connector
            "Pd",  # Punctuation dash
            "Pi",  # Punctuation initial (quotes etc.)
            "Pf",  # Punctuation final (quotes etc.)
            "Zs",  # Separator space
        }
        for ch in full_name:
            if unicodedata.category(ch) not in valid_categories:
                return ch

        return None

    @staticmethod
    def create(tx: Transaction, username: str, full_name: str) -> User:
        """
        Create a new user.
        """
        assert User.is_valid_username(username)
        assert User.is_valid_full_name(full_name)
        user_id, created_at = tx.execute_fetch_one(
            'INSERT INTO "user" DEFAULT VALUES RETURNING id, created_at;',
        )
        tx.execute(
            "INSERT INTO user_username (user_id, username) VALUES (%s, %s);",
            (user_id, username),
        )
        tx.execute(
            "INSERT INTO user_full_name (user_id, full_name) VALUES (%s, %s);",
            (user_id, full_name),
        )
        return User(user_id, username, full_name, created_at)

    @staticmethod
    def get_by_username(tx: Transaction, username: str) -> Optional[User]:
        result: Optional[Tuple[int]] = tx.execute_fetch_optional(
            """
            SELECT user_id
            FROM user_username
            WHERE username = %s
            """,
            (username,),
        )
        if result is None:
            return None
        else:
            return User.get_by_id(tx, result[0])

    @staticmethod
    def get_by_id(tx: Transaction, user_id: int) -> Optional[User]:
        result: Optional[Tuple[int, str, str, datetime]] = tx.execute_fetch_optional(
            """
            SELECT
              id,
              user_current_username(id),
              user_current_full_name(id),
              created_at
            FROM
              "user"
            WHERE
              id = %s;
            """,
            (user_id,),
        )
        if result is None:
            return None
        else:
            # We told the typechecker that none of these fields are null,
            # but due to the way the database schema is set up, this is not
            # guaranteed by the database.
            assert all(field is not None for field in result)
            return User(*result)

    @staticmethod
    def list_all(tx: Transaction) -> Iterable[User]:
        for result in tx.execute_fetch_all(
            """
            SELECT
              id,
              user_current_username(id),
              user_current_full_name(id),
              created_at
            FROM
              "user"
            ORDER BY
              -- TODO: Order by net worth, once we have that.
              created_at ASC;
            """,
        ):
            assert all(field is not None for field in result)
            yield User(*result)
