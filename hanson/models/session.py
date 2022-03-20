from __future__ import annotations

from typing import NamedTuple, Optional, Tuple
from datetime import datetime
from uuid import UUID, uuid4

from hanson.database import Transaction


class Session(NamedTuple):
    id: int
    user_id: int
    token: UUID
    created_at: datetime
    expires_at: datetime

    @staticmethod
    def create(tx: Transaction, user_id: int) -> Session:
        """
        Create a new session for the given user.
        """
        token = uuid4()
        with tx.cursor() as cur:
            cur.execute(
                """
                INSERT INTO "session" (user_id, token, expires_at)
                VALUES (%s, %s, now() + '30d'::INTERVAL)
                RETURNING id, created_at, expires_at;
                """,
                (user_id, token),
            )
            result: Tuple[int, datetime, datetime] = cur.fetchone()
            session_id, created_at, expires_at = result
            assert created_at.tzinfo is not None
            assert expires_at.tzinfo is not None
            return Session(session_id, user_id, token, created_at, expires_at)

    @staticmethod
    def get_by_token_not_expired(tx: Transaction, token: UUID) -> Optional[Session]:
        """
        Check if a non-expired session exists for the given token, if so return it.
        """
        with tx.cursor() as cur:
            cur.execute(
                """
                SELECT id, user_id, created_at, expires_at
                FROM "session"
                WHERE token = %s AND expires_at > now();
                """,
                (token,),
            )
            result: Optional[Tuple[int, int, datetime, datetime]] = cur.fetchone()
            if result is None:
                return None
            session_id, user_id, created_at, valid_until = result
            assert created_at.tzinfo is not None
            assert valid_until.tzinfo is not None
            return Session(session_id, user_id, token, created_at, valid_until)

    @staticmethod
    def get_from_cookie(tx: Transaction) -> Optional[Session]:
        """
        If the "session" cookie is set (in Flask), and it contains a valid session token,
        return that session.
        """
        from flask import request

        token_str = request.cookies.get("session")

        if token_str is None:
            return None

        try:
            token = UUID(token_str)
        except ValueError:
            return None

        return Session.get_by_token_not_expired(tx, token)
