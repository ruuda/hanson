from __future__ import annotations

from typing import Iterable, NamedTuple, Optional, Tuple
from dataclasses import dataclass
from decimal import Decimal

from hanson.models.currency import Amount, Points, OutcomeShares
from hanson.database import Transaction


class UserAccount(NamedTuple):
    id: int
    balance: Amount
    market_id: Optional[int]

    @staticmethod
    def list_all_for_user(tx: Transaction, user_id: int) -> Iterable[UserAccount]:
        """
        List all the accounts that the user has, ordered by market (so they can
        be grouped in a single pass), with the points account (that has no
        market) at the start.
        """
        with tx.cursor() as cur:
            cur.execute(
                """
                SELECT
                  account.id,
                  account.type,
                  account.outcome_id,
                  account_current_balance(account.id),
                  outcome.market_id
                FROM
                  account
                LEFT OUTER JOIN
                  outcome ON account.outcome_id = outcome.id
                WHERE
                  owner_user_id = %s
                ORDER BY
                  outcome.market_id NULLS FIRST
                """,
                (user_id,),
            )
            result: Optional[Tuple[
                int, str, Optional[int], Decimal, Optional[int]
            ]] = cur.fetchone()

            while result is not None:
                id, account_type, outcome_id, balance, market_id = result
                if account_type == 'points':
                    assert outcome_id is None
                    assert market_id is None
                    yield UserAccount(
                        id=id,
                        balance=Points(balance),
                        market_id=None,
                    )
                elif account_type == 'outcome_shares':
                    assert outcome_id is not None
                    assert market_id is not None
                    yield UserAccount(
                        id=id,
                        balance=OutcomeShares(balance, outcome_id),
                        market_id=market_id,
                    )
                else:
                    raise Exception("Invalid account type.")


def get_user_points_balance(tx: Transaction, user_id: int) -> Optional[Points]:
    with tx.cursor() as cur:
        cur.execute(
            """
            SELECT account_current_balance(id)
            FROM   account
            WHERE  owner_user_id = %s AND type = 'points'
            """,
            (user_id,),
        )
        result = cur.fetchone()
        if result is not None:
            return Points(result[0])
        else:
            return None


def get_market_points_balance(tx: Transaction, market_id: int) -> Optional[Points]:
    with tx.cursor() as cur:
        cur.execute(
            """
            SELECT account_current_balance(id)
            FROM   account
            WHERE  owner_market_id = %s AND type = 'points'
            """,
            (market_id,),
        )
        result = cur.fetchone()
        if result is not None:
            return Points(result[0])
        else:
            return None


def get_user_share_balance(tx: Transaction, user_id: int, outcome_id: int) -> Optional[OutcomeShares]:
    with tx.cursor() as cur:
        cur.execute(
            """
            SELECT
              account_current_balance(id)
            FROM
              account
            WHERE
              owner_user_id = %s
              AND outcome_id = %s
              AND type = 'outcome_shares'
            """,
            (user_id, outcome_id),
        )
        result = cur.fetchone()
        if result is not None:
            return OutcomeShares(result[0], outcome_id)
        else:
            return None


def get_market_share_balance(tx: Transaction, market_id: int, outcome_id: int) -> Optional[OutcomeShares]:
    with tx.cursor() as cur:
        cur.execute(
            """
            SELECT
              account_current_balance(id)
            FROM
              account
            WHERE
              owner_market_id = %s
              AND outcome_id = %s
              AND type = 'outcome_shares'
            """,
            (market_id, outcome_id),
        )
        result = cur.fetchone()
        if result is not None:
            return OutcomeShares(result[0], outcome_id)
        else:
            return None
