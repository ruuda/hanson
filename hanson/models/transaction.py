# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from datetime import datetime
from decimal import Decimal
from typing import List, Optional, Tuple

from hanson.models.account import Balance
from hanson.database import Transaction
from hanson.models.account import MarketAccount, UserAccount
from hanson.models.currency import Points, Shares
from hanson.models.outcome import Outcome


def create_transaction_income(
    tx: Transaction,
    user_id: int,
    amount: Points,
    now: Optional[datetime] = None,
) -> int:
    """
    Create a new transaction that brings `amount` new points into existence and
    puts them in the user's account. If the user has no account yet, one will be
    created.

    The transaction timestamp defaults to the database's current time, but can
    optionally be specified using the `now` argument.
    """
    if now is not None:
        assert now.tzinfo is not None

    account = UserAccount.ensure_points_account(tx, user_id)

    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO transactions (type, created_at)
        VALUES ('income', COALESCE(%s, now())) RETURNING id;
        """,
        (now,),
    )

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO subtransactions (type, transaction_id)
        VALUES ('income', %s)
        RETURNING id;
        """,
        (transaction_id,),
    )

    mutation_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO mutations (subtransaction_id, debit_account_id, amount)
        VALUES (%s, %s, %s)
        RETURNING id;
        """,
        (subtransaction_id, account.id, amount.amount),
    )

    post_balance: Decimal = tx.execute_fetch_scalar(
        """
        INSERT INTO
          account_balances (account_id, mutation_id, post_balance)
        VALUES
          ( %(account_id)s
          , %(mutation_id)s
          , (SELECT current_balance FROM accounts_ext WHERE id = %(account_id)s) + %(amount)s
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
    user_account: UserAccount[Balance],
    market_account: MarketAccount[Balance],
    amount: Balance,
) -> int:
    """
    Move `amount` points from the user's account into the market's account.
    Can also go the other way if `amount` is negative. Returns the mutation id.
    """
    if amount.amount > Decimal("0.00"):
        credit_account_id = user_account.id
        debit_account_id = market_account.id
        amount_abs = amount.amount
    elif amount.amount < Decimal("0.00"):
        credit_account_id = market_account.id
        debit_account_id = user_account.id
        amount_abs = -amount.amount
    else:
        raise ValueError("Please do not pollute the database with 0-valued transfers.")

    mutation_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO mutations
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
            amount_abs,
        ),
    )
    post_balances = list(
        tx.execute_fetch_all(
            """
            INSERT INTO
              account_balances (account_id, mutation_id, post_balance)
            VALUES
              ( %(user_account_id)s
              , %(mutation_id)s
              , (SELECT current_balance FROM accounts_ext where id = %(user_account_id)s) - %(amount)s
              ),
              ( %(market_account_id)s
              , %(mutation_id)s
              , (SELECT current_balance FROM accounts_ext where id = %(market_account_id)s) + %(amount)s
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
    assert post_balances[0][0] == user_account.balance.amount - amount.amount, (
        "Transfer does not balance! "
        f"Post balance in database: {post_balances[0][0]}; "
        f"User pre balance: {user_account.balance}; "
        f"Amount: {amount}"
    )
    assert post_balances[1][0] == market_account.balance.amount + amount.amount

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
    # Due to the canonical representation constraint, we have to distinguish
    # positive and negative mutations here.
    if amount.amount > 0:
        mutation_id = tx.execute_fetch_scalar(
            """
            INSERT INTO mutations
              ( subtransaction_id
              , debit_account_id
              , amount
              )
            VALUES (%s, %s, %s)
            RETURNING id;
            """,
            (subtransaction_id, debit_account_id, amount.amount),
        )
    else:
        mutation_id = tx.execute_fetch_scalar(
            """
            INSERT INTO mutations
              ( subtransaction_id
              , credit_account_id
              , amount
              )
            VALUES (%s, %s, %s)
            RETURNING id;
            """,
            (subtransaction_id, debit_account_id, -amount.amount),
        )

    post_balance: Decimal = tx.execute_fetch_scalar(
        """
        INSERT INTO
          account_balances (account_id, mutation_id, post_balance)
        VALUES
          ( %(debit_account_id)s
          , %(mutation_id)s
          , (SELECT current_balance FROM accounts_ext WHERE id = %(debit_account_id)s) + %(amount)s
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
    now: Optional[datetime] = None,
) -> int:
    """
    Create a transaction that first converts `amount` of the user's points into
    outcome shares, and then puts all of the outcome shares into the market's
    AMM pool.

    The transaction timestamp defaults to the database's current time, but can
    optionally be specified using the `now` argument.
    """
    if now is not None:
        assert now.tzinfo is not None

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
        INSERT INTO transactions (type, created_at)
        VALUES ('fund_market', COALESCE(%s, now())) RETURNING id;
        """,
        (now,),
    )

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO subtransactions (type, transaction_id)
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
        INSERT INTO subtransactions (type, transaction_id)
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


def create_subtransaction_trade(
    tx: Transaction,
    transaction_id: int,
    debit_user_id: int,
    credit_market_id: int,
    amounts: List[Shares],
) -> int:
    """
    Create a subtransaction where a user trades against the pool.
    """
    outcomes = Outcome.get_all_by_market(tx, credit_market_id).outcomes
    assert len(outcomes) == len(amounts)

    subtransaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO subtransactions (type, transaction_id)
        VALUES ('trade', %s)
        RETURNING id;
        """,
        (transaction_id,),
    )

    for outcome, amount in zip(outcomes, amounts):
        assert amount.outcome_id == outcome.id

        if amount.is_zero():
            continue

        user_share_account = UserAccount.expect_share_account(
            tx,
            user_id=debit_user_id,
            market_id=credit_market_id,
            outcome_id=outcome.id,
        )
        pool_share_account = MarketAccount.expect_pool_account(
            tx,
            market_id=credit_market_id,
            outcome_id=outcome.id,
        )
        create_mutation_transfer_user_market(
            tx,
            subtransaction_id=subtransaction_id,
            user_account=user_share_account,
            market_account=pool_share_account,
            amount=amount,
        )

    return subtransaction_id


def create_transaction_execute_order(
    tx: Transaction,
    debit_user_id: int,
    credit_market_id: int,
    cost: Points,
    amounts: List[Shares],
    now: Optional[datetime] = None,
) -> int:
    """
    Create a transaction that exchanges the given amount of shares.

    The transaction timestamp defaults to the database's current time, but can
    optionally be specified using the `now` argument.
    """
    if now is not None:
        assert now.tzinfo is not None

    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO transactions (type, created_at)
        VALUES ('trade', COALESCE(%s, now())) RETURNING id;
        """,
        (now,),
    )
    if cost.amount > 0:
        # If the cost is positive, first exchange so we have the shares to trade.
        create_subtransaction_exchange_points_to_shares(
            tx,
            transaction_id=transaction_id,
            user_id=debit_user_id,
            market_id=credit_market_id,
            amount=cost,
        )

    create_subtransaction_trade(
        tx,
        transaction_id=transaction_id,
        debit_user_id=debit_user_id,
        credit_market_id=credit_market_id,
        amounts=amounts,
    )

    if cost.amount < 0:
        # If the cost is negative, we first trade, so we have the shares to destroy.
        create_subtransaction_exchange_points_to_shares(
            tx,
            transaction_id=transaction_id,
            user_id=debit_user_id,
            market_id=credit_market_id,
            amount=cost,
        )
    return transaction_id


def create_transaction_annihilate(
    tx: Transaction,
    user_id: int,
    market_id: int,
    amount: Points,
) -> int:
    """
    Create an 'annihilate' transaction that annihilates outcome shares and turns
    them back into points. `amount` is subtracted from the balance of every
    share account, and added to the balance of the user's points account.
    """
    transaction_id: int = tx.execute_fetch_scalar(
        """
        INSERT INTO transactions (type) VALUES ('annihilate') RETURNING id;
        """
    )
    create_subtransaction_exchange_points_to_shares(
        tx,
        transaction_id=transaction_id,
        user_id=user_id,
        market_id=market_id,
        amount=-amount,
    )
    return transaction_id
