from hanson.models.account import MarketAccount, UserAccount
from hanson.models.user import User
from hanson.models.market import Market
from hanson.models.outcome import Outcome
from hanson.database import Transaction


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
