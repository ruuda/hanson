from __future__ import annotations

from decimal import Decimal
from typing import Generic, Iterable, Optional, Tuple, TypeVar
from dataclasses import dataclass

from hanson.database import Transaction
from hanson.models.currency import Amount, Shares, Points


Balance = TypeVar("Balance", bound=Amount)


@dataclass(frozen=True)
class UserAccount(Generic[Balance]):
    """
    An account, that holds points our outcome shares, which belongs to a user.
    """

    id: int
    balance: Balance
    market_id: Optional[int] = None

    @staticmethod
    def list_all_for_user(
        tx: Transaction, user_id: int
    ) -> Iterable[UserAccount[Amount]]:
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
            elif account_type == "shares":
                assert outcome_id is not None
                assert market_id is not None
                yield UserAccount(
                    id=id,
                    balance=Shares(balance, outcome_id),
                    market_id=market_id,
                )
            else:
                raise Exception("Invalid account type.")

    @staticmethod
    def get_points_account(tx: Transaction, user_id: int) -> Optional[UserAccount[Points]]:
        """
        Return the points account for the given user, if it exists.
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
        else:
            return None

    @staticmethod
    def ensure_points_account(tx: Transaction, user_id: int) -> UserAccount[Points]:
        """
        Return the points account for the given user,
        or create it if it doesn't yet exist.
        """
        result = UserAccount.get_points_account(tx, user_id)

        if result is not None:
            return result

        account_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO account (type, owner_user_id) VALUES ('points', %s)
            RETURNING id;
            """,
            (user_id,),
        )
        return UserAccount(id=account_id, balance=Points.zero())

    @staticmethod
    def get_share_account(
        tx: Transaction,
        *,
        user_id: int,
        market_id: int,
        outcome_id: int,
    ) -> Optional[UserAccount[Shares]]:
        result: Optional[Tuple[int, Decimal]] = tx.execute_fetch_optional(
            """
            SELECT account.id, COALESCE(account_current_balance(account.id), 0.00)
              FROM account
             WHERE type = 'shares'
               AND outcome_id = %s
               AND owner_user_id = %s
            """,
            (outcome_id, user_id),
        )
        if result is not None:
            return UserAccount(
                id=result[0],
                balance=Shares(result[1], outcome_id),
                market_id=market_id,
            )
        else:
            return None

    @staticmethod
    def expect_share_account(
        tx: Transaction, *, user_id: int, market_id: int, outcome_id: int
    ) -> UserAccount[Shares]:
        result = UserAccount.get_share_account(
            tx,
            user_id=user_id,
            market_id=market_id,
            outcome_id=outcome_id,
        )
        assert result is not None
        return result

    @staticmethod
    def ensure_share_account(
        tx: Transaction,
        *,
        user_id: int,
        market_id: int,
        outcome_id: int,
    ) -> UserAccount[Shares]:
        """
        Return the outcome shares account for the given user, or create it if it
        doesn't yet exist.
        """
        result = UserAccount.get_share_account(
            tx,
            user_id=user_id,
            market_id=market_id,
            outcome_id=outcome_id,
        )
        if result is not None:
            return result

        account_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO account (type, outcome_id, owner_user_id)
            VALUES ('shares', %s, %s)
            RETURNING id;
            """,
            (outcome_id, user_id),
        )
        return UserAccount(
            id=account_id,
            balance=Shares.zero(outcome_id),
            market_id=market_id,
        )


@dataclass(frozen=True)
class MarketAccount(Generic[Balance]):
    """
    An account, that holds points our outcome shares, which belongs to a market.
    """

    id: int
    balance: Balance
    market_id: int

    @staticmethod
    def get_points_account(
        tx: Transaction, market_id: int
    ) -> Optional[MarketAccount[Points]]:
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
        else:
            return None

    @staticmethod
    def expect_points_account(tx: Transaction, market_id: int) -> MarketAccount[Points]:
        result = MarketAccount.get_points_account(tx, market_id)
        assert result is not None
        return result

    @staticmethod
    def ensure_points_account(tx: Transaction, market_id: int) -> MarketAccount[Points]:
        """
        Return the points account for the given market,
        or create it if it doesn't yet exist.
        """
        result = MarketAccount.get_points_account(tx, market_id)
        if result is not None:
            return result

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

    @staticmethod
    def get_pool_account(
        tx: Transaction, market_id: int, outcome_id: int
    ) -> Optional[MarketAccount[Shares]]:
        result: Optional[Tuple[int, Decimal]] = tx.execute_fetch_optional(
            """
            SELECT account.id, COALESCE(account_current_balance(account.id), 0.00)
              FROM account
             WHERE type = 'shares'
               AND outcome_id = %s
               AND owner_market_id = %s
            """,
            (outcome_id, market_id),
        )
        if result is not None:
            return MarketAccount(
                id=result[0],
                balance=Shares(result[1], outcome_id),
                market_id=market_id,
            )
        else:
            return None

    @staticmethod
    def expect_pool_account(
        tx: Transaction, market_id: int, outcome_id: int
    ) -> MarketAccount[Shares]:
        result = MarketAccount.get_pool_account(tx, market_id, outcome_id)
        assert result is not None
        return result

    @staticmethod
    def ensure_pool_account(
        tx: Transaction, market_id: int, outcome_id: int
    ) -> MarketAccount[Shares]:
        """
        Return the outcome shares account for the given market (for use by the
        market maker), or create it if it doesn't yet exist.
        """
        result = MarketAccount.get_pool_account(tx, market_id, outcome_id)
        if result is not None:
            return result

        account_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO account (type, outcome_id, owner_market_id)
            VALUES ('shares', %s, %s)
            RETURNING id;
            """,
            (outcome_id, market_id),
        )
        return MarketAccount(
            id=account_id,
            balance=Shares.zero(outcome_id),
            market_id=market_id,
        )


def get_user_share_balance(
    tx: Transaction, user_id: int, outcome_id: int
) -> Optional[Shares]:
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
              AND type = 'shares'
            """,
            (user_id, outcome_id),
        )
        result = cur.fetchone()
        if result is not None:
            return Shares(result[0], outcome_id)
        else:
            return None


def get_market_share_balance(
    tx: Transaction, market_id: int, outcome_id: int
) -> Optional[Shares]:
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
              AND type = 'shares'
            """,
            (market_id, outcome_id),
        )
        result = cur.fetchone()
        if result is not None:
            return Shares(result[0], outcome_id)
        else:
            return None
