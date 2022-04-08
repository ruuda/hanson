from __future__ import annotations

from decimal import Decimal
from typing import Iterable, NamedTuple, Optional, Tuple

from hanson.database import Transaction
from hanson.models.currency import Amount, OutcomeShares, Points


class UserAccount(NamedTuple):
    """
    An account, that holds points our outcome shares, which belongs to a user.
    """

    id: int
    balance: Amount
    market_id: Optional[int] = None

    @staticmethod
    def list_all_for_user(tx: Transaction, user_id: int) -> Iterable[UserAccount]:
        """
        List all the accounts that the user has, ordered by market (so they can
        be grouped in a single pass), with the points account (that has no
        market) at the start.
        """
        id: int
        account_type: str
        outcome_id: Optional[int]
        balance: Decimal
        market_id: Optional[int]
        for id, account_type, outcome_id, balance, market_id in tx.execute_fetch_all(
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
        ):
            if account_type == "points":
                assert outcome_id is None
                assert market_id is None
                yield UserAccount(
                    id=id,
                    balance=Points(balance),
                    market_id=None,
                )
            elif account_type == "outcome_shares":
                assert outcome_id is not None
                assert market_id is not None
                yield UserAccount(
                    id=id,
                    balance=OutcomeShares(balance, outcome_id),
                    market_id=market_id,
                )
            else:
                raise Exception("Invalid account type.")

    @staticmethod
    def ensure_points_account(tx: Transaction, user_id: int) -> UserAccount:
        """
        Return the points account for the given user,
        or create it if it doesn't yet exist.
        """
        result: Optional[Tuple[int, Decimal]] = tx.execute_fetch_optional(
            """
            SELECT account.id, COALESCE(account_current_balance(account.id), 0.00)
            FROM   account
            WHERE  type = 'points' AND owner_user_id = %s
            """,
            (user_id,),
        )
        if result is not None:
            return UserAccount(id=result[0], balance=Points(result[1]))

        account_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO account (type, owner_user_id) VALUES ('points', %s)
            RETURNING id;
            """,
            (user_id,),
        )
        return UserAccount(id=account_id, balance=Points.zero())


class MarketAccount(NamedTuple):
    """
    An account, that holds points our outcome shares, which belongs to a market.
    """

    id: int
    balance: Amount
    market_id: int

    @staticmethod
    def ensure_points_account(tx: Transaction, market_id: int) -> MarketAccount:
        """
        Return the points account for the given market,
        or create it if it doesn't yet exist.
        """
        result: Optional[Tuple[int, Decimal]] = tx.execute_fetch_optional(
            """
            SELECT account.id, COALESCE(account_current_balance(account.id), 0.00)
            FROM   account
            WHERE  type = 'points' AND owner_market_id = %s
            """,
            (market_id,),
        )
        if result is not None:
            return MarketAccount(
                id=result[0],
                balance=Points(result[1]),
                market_id=market_id,
            )

        account_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO account (type, owner_market_id) VALUES ('points', %s)
            RETURNING id;
            """,
            (market_id,),
        )
        return MarketAccount(
            id=account_id,
            balance=Points.zero(),
            market_id=market_id,
        )


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


def get_user_share_balance(
    tx: Transaction, user_id: int, outcome_id: int
) -> Optional[OutcomeShares]:
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


def get_market_share_balance(
    tx: Transaction, market_id: int, outcome_id: int
) -> Optional[OutcomeShares]:
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
