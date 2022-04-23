from __future__ import annotations

from decimal import Decimal
from typing import Tuple

from hanson.database import Transaction
from hanson.models.account import MarketAccount, UserAccount
from hanson.models.currency import Points, Shares
from hanson.models.outcome import Outcome


def create_transaction_income(
    tx: Transaction,
    user_id: int,
    amount: Points,
) -> int:
    """
    Create a new transaction that brings `amount` new points into existence and
    puts them in the user's account. If the user has no account yet, one will be
    created.
    """
    account = UserAccount.ensure_points_account(tx, user_id)

    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "transaction" (type) VALUES ('income') RETURNING id;
        """
    )

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "subtransaction" (type, transaction_id)
        VALUES ('income', %s)
        RETURNING id;
        """,
        (transaction_id,),
    )

    mutation_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "mutation" (subtransaction_id, debit_account_id, amount)
        VALUES (%s, %s, %s)
        RETURNING id;
        """,
        (subtransaction_id, account.id, amount.amount),
    )

    post_balance: Decimal = tx.execute_fetch_scalar(
        """
        INSERT INTO
          "account_balance" (account_id, mutation_id, post_balance)
        VALUES
          ( %(account_id)s
          , %(mutation_id)s
          , COALESCE(account_current_balance(%(account_id)s), 0.00) + %(amount)s
          )
        RETURNING
          post_balance;
        """,
        {
            "account_id": account.id,
            "mutation_id": mutation_id,
            "amount": amount.amount,
        },
    )
    assert amount + account.balance == Points(post_balance)

    return transaction_id


def create_mutation_transfer_user_market(
    tx: Transaction,
    *,
    subtransaction_id: int,
    user_account: UserAccount[Points],
    market_account: MarketAccount[Points],
    amount: Points,
) -> int:
    """
    Move `amount` points from the user's account into the market's account.
    Can also go the other way if `amount` is negative. Returns the mutation id.
    """
    if amount > Points.zero():
        subtransaction_type = "exchange_create_shares"
        credit_account_id = user_account.id
        debit_account_id = market_account.id
        amount_abs = amount
    else:
        subtransaction_type = "exchange_destroy_shares"
        credit_account_id = market_account.id
        debit_account_id = user_account.id
        amount_abs = -amount

    mutation_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "mutation"
          ( subtransaction_id
          , credit_account_id
          , debit_account_id
          , amount
          )
        VALUES (%s, %s, %s, %s)
        RETURNING id;
        """,
        (
            subtransaction_id,
            credit_account_id,
            debit_account_id,
            amount_abs.amount,
        ),
    )
    post_balances = list(
        tx.execute_fetch_all(
            """
            INSERT INTO
              "account_balance" (account_id, mutation_id, post_balance)
            VALUES
              ( %(user_account_id)s
              , %(mutation_id)s
              , COALESCE(account_current_balance(%(user_account_id)s), 0.00) - %(amount)s
              ),
              ( %(market_account_id)s
              , %(mutation_id)s
              , COALESCE(account_current_balance(%(market_account_id)s), 0.00) + %(amount)s
              )
            RETURNING
              post_balance;
            """,
            {
                "user_account_id": user_account.id,
                "market_account_id": market_account.id,
                "mutation_id": mutation_id,
                "amount": amount.amount,
            },
        )
    )
    # We should only fund the market at initialization time, which means the
    # pool accounts should now all contain `amount` of outcome shares.
    # (We initialize to a uniform distribution.)
    assert post_balances[0][0] == (user_account.balance - amount).amount
    assert post_balances[1][0] == (market_account.balance + amount).amount

    return mutation_id


def create_mutation_mint_shares(
    tx: Transaction,
    *,
    subtransaction_id: int,
    debit_account_id: int,
    amount: Shares,
) -> Tuple[int, Shares]:
    """
    Create `amount` new outcome shares in the debit account. Returns the
    mutation id and the post balance.
    """
    # TODO: Deal with negative amounts.
    mutation_id = tx.execute_fetch_scalar(
        """
        INSERT INTO "mutation"
          ( subtransaction_id
          , debit_account_id
          , amount
          )
        VALUES (%s, %s, %s)
        RETURNING id;
        """,
        (subtransaction_id, debit_account_id, amount.amount),
    )
    post_balance: Decimal = tx.execute_fetch_scalar(
        """
        INSERT INTO
          "account_balance" (account_id, mutation_id, post_balance)
        VALUES
          ( %(pool_account_id)s
          , %(mutation_id)s
          , COALESCE(account_current_balance(%(debit_account_id)s), 0.00) + %(amount)s
          )
        RETURNING
          post_balance;
        """,
        {
            "debit_account_id": debit_account_id,
            "mutation_id": mutation_id,
            "amount": amount.amount,
        },
    )
    return mutation_id, Shares(post_balance, amount.outcome_id)


def create_transaction_fund_market(
    tx: Transaction,
    user_id: int,
    market_id: int,
    amount: Points,
) -> int:
    """
    Create a transaction that first converts `amount` of the user's points into
    outcome shares, and then puts all of the outcome shares into the market's
    AMM pool.
    """
    user_points_account = UserAccount.ensure_points_account(tx, user_id)
    assert (
        user_points_account.balance >= amount
    ), "User cannot afford to fund the market."

    market_points_account = MarketAccount.ensure_points_account(tx, market_id)
    outcomes = Outcome.get_all_by_market(tx, market_id).outcomes
    pool_accounts = [
        MarketAccount.ensure_pool_account(tx, market_id, outcome.id)
        for outcome in outcomes
    ]

    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "transaction" (type) VALUES ('fund_market') RETURNING id;
        """
    )

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "subtransaction" (type, transaction_id)
        VALUES ('fund_market', %s)
        RETURNING id;
        """,
        (transaction_id,),
    )

    # Move the points from the user's account into the market's account.
    create_mutation_transfer_user_market(
        tx,
        subtransaction_id=subtransaction_id,
        user_account=user_points_account,
        market_account=market_points_account,
        amount=amount,
    )

    # Mint outcome shares in equal amounts, and put them in the pool.
    for outcome, pool_account in zip(outcomes, pool_accounts):
        _mutation_id, post_balance = create_mutation_mint_shares(
            tx,
            subtransaction_id=subtransaction_id,
            debit_account_id=pool_account.id,
            amount=Shares(amount.amount, outcome.id),
        )
        assert post_balance.amount == amount.amount

    return transaction_id


def create_subtransaction_exchange_points_to_shares(
    tx: Transaction,
    transaction_id: int,
    user_id: int,
    market_id: int,
    amount: Points,
) -> int:
    """
    Create a transaction that converts `amount` of the user's points into
    outcome shares. The amount can be negative to convert shares back to points,
    but it must not be zero.
    """
    assert (
        amount != Points.zero()
    ), "Please don't pollute the database with 0-point exchanges."
    user_points_account = UserAccount.ensure_points_account(tx, user_id)
    market_points_account = MarketAccount.ensure_points_account(tx, market_id)

    assert (
        user_points_account.balance - amount >= Points.zero()
    ), "User cannot afford to buy so many shares."
    assert (
        market_points_account.balance + amount >= Points.zero()
    ), "There shouldn’t be that many outstanding shares."

    outcomes = Outcome.get_all_by_market(tx, market_id).outcomes
    share_accounts = [
        UserAccount.ensure_share_account(
            tx,
            user_id=user_id,
            market_id=market_id,
            outcome_id=outcome.id,
        )
        for outcome in outcomes
    ]

    if amount > Points.zero():
        subtransaction_type = "exchange_create_shares"
    else:
        subtransaction_type = "exchange_destroy_shares"

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO "subtransaction" (type, transaction_id)
        VALUES (%s, %s)
        RETURNING id;
        """,
        (
            subtransaction_type,
            transaction_id,
        ),
    )

    # Move the points from the user's account into the market's account.
    create_mutation_transfer_user_market(
        tx,
        subtransaction_id=subtransaction_id,
        user_account=user_points_account,
        market_account=market_points_account,
        amount=amount,
    )

    # Then put the new shares in the user's account.
    for outcome, account in zip(outcomes, share_accounts):
        mutation_id, post_balance = create_mutation_mint_shares(
            tx,
            subtransaction_id=subtransaction_id,
            debit_account_id=account.id,
            amount=Shares(amount.amount, outcome.id),
        )
        assert post_balance >= Shares.zero(
            outcome.id
        ), "User must have enough shares to be able to destroy them."

    return subtransaction_id
