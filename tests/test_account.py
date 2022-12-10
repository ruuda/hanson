from decimal import Decimal

from hanson.database import Transaction
from hanson.models.account import MarketAccount, UserAccount
from hanson.models.currency import Points
from hanson.models.market import Market
from hanson.models.outcome import Outcome
from hanson.models.user import User


def test_user_account_ensure_points_account_is_idempotent(
    tx: Transaction,
    user: User,
) -> None:
    account_1 = UserAccount.ensure_points_account(tx, user.id)
    account_2 = UserAccount.ensure_points_account(tx, user.id)
    assert account_1 == account_2


def test_market_account_ensure_points_account_is_idempotent(
    tx: Transaction,
    market: Market,
) -> None:
    account_1 = MarketAccount.ensure_points_account(tx, market.id)
    account_2 = MarketAccount.ensure_points_account(tx, market.id)
    assert account_1 == account_2


def test_market_account_ensure_pool_account_is_idempotent(
    tx: Transaction,
    market: Market,
) -> None:
    outcome_1 = Outcome.create_discrete(tx, market.id, "All", "#000000")
    account_1 = MarketAccount.ensure_pool_account(tx, market.id, outcome_1.id)
    account_2 = MarketAccount.ensure_pool_account(tx, market.id, outcome_1.id)
    assert account_1 == account_2


def test_mutation_mint_shares_handles_negative_numbers(
    tx: Transaction,
    user: User,
    market: Market,
) -> None:
    from hanson.models.transaction import (
        create_subtransaction_exchange_points_to_shares,
    )
    from hanson.models.transaction import create_transaction_income

    o1 = Outcome.create_discrete(tx, market.id, "Black", "#000000")
    o2 = Outcome.create_discrete(tx, market.id, "White", "#ffffff")

    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "transaction" (type) VALUES ('annihilate') RETURNING id;
        """
    )

    create_transaction_income(tx, user_id=user.id, amount=Points(Decimal("1.00")))

    # Step 1, turn the points into shares.
    create_subtransaction_exchange_points_to_shares(
        tx,
        transaction_id=transaction_id,
        user_id=user.id,
        market_id=market.id,
        amount=Points(Decimal("1.00")),
    )

    balance_o1 = UserAccount.get_share_account(
        tx, user_id=user.id, market_id=market.id, outcome_id=o1.id
    )
    balance_o2 = UserAccount.get_share_account(
        tx, user_id=user.id, market_id=market.id, outcome_id=o2.id
    )
    balance_pt = UserAccount.get_points_account(tx, user_id=user.id)
    assert balance_o1.balance.amount == Decimal("1.00")
    assert balance_o2.balance.amount == Decimal("1.00")
    assert balance_pt.balance.amount == Decimal("0.00")

    # Step 2, turn the shares into points.
    create_subtransaction_exchange_points_to_shares(
        tx,
        transaction_id=transaction_id,
        user_id=user.id,
        market_id=market.id,
        amount=Points(-Decimal("1.00")),
    )
    balance_o1 = UserAccount.get_share_account(
        tx, user_id=user.id, market_id=market.id, outcome_id=o1.id
    )
    balance_o2 = UserAccount.get_share_account(
        tx, user_id=user.id, market_id=market.id, outcome_id=o2.id
    )
    balance_pt = UserAccount.get_points_account(tx, user_id=user.id)
    assert balance_o1.balance.amount == Decimal("0.00")
    assert balance_o2.balance.amount == Decimal("0.00")
    assert balance_pt.balance.amount == Decimal("1.00")
