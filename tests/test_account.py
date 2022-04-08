from hanson.models.account import UserAccount
from hanson.models.user import User
from hanson.database import Transaction


def test_user_account_ensure_points_account_is_idempotent(
    tx: Transaction,
    user: User,
) -> None:
    account_1 = UserAccount.ensure_points_account(tx, user.id)
    account_2 = UserAccount.ensure_points_account(tx, user.id)
    assert account_1 == account_2
