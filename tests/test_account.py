from hanson.models.account import MarketAccount, UserAccount
from hanson.models.user import User
from hanson.models.market import Market
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
