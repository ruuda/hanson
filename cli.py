#!/usr/bin/env python

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Management CLI for a Hanson instance.
"""

import click

from decimal import Decimal
from typing import ContextManager

from hanson.database import Transaction, connect_config
from hanson.models.config import Config


def begin_transaction(ctx: click.Context) -> ContextManager[Transaction]:
    config: Config = ctx.obj
    conn = connect_config(config)
    return conn.begin()


@click.group()
@click.option(
    "--config",
    type=str,
    default="config.toml",
    help="Path to the config toml file.",
    show_default=True,
)
@click.pass_context
def main(ctx: click.Context, config: str) -> None:
    ctx.obj = Config.load_from_toml_file(config)


@main.command()
@click.argument("username", required=True, type=str)
@click.argument("full_name", required=True, type=str)
@click.pass_context
def add_user(ctx: click.Context, username: str, full_name: str) -> None:
    """
    Create a new user. Mostly useful for testing purposes.
    """
    from hanson.models.user import User
    from hanson.models.account import UserAccount

    with begin_transaction(ctx) as tx:
        user = User.create(tx, username, full_name)
        UserAccount.ensure_points_account(tx, user.id)
        tx.commit()
        print(f"Created user with id {user.id} with username {user.username}.")


@main.command()
@click.argument("points_per_user", required=True, type=Decimal)
@click.pass_context
def airdrop(ctx: click.Context, points_per_user: Decimal) -> None:
    """
    Create points out of thin air for every user.
    """
    from hanson.models.currency import Points
    from hanson.models.transaction import create_transaction_income
    from hanson.models.user import User

    amount = Points(points_per_user)

    with begin_transaction(ctx) as tx:
        for user in User.list_all(tx):
            _transaction_id = create_transaction_income(
                tx,
                user_id=user.id,
                amount=amount,
            )
            print(f"{user.username}: +{amount.amount} points")

        tx.commit()


@main.command()
@click.pass_context
def annihilate(ctx: click.Context) -> None:
    """
    Ensure no users own shares in all outcomes of a market.

    If users have outcome shares in every possible outcome of a market, then
    effectively they have points, so destroy the excess outcome shares, and
    convert them back to points.

    This functionality exists only to recover from a bad state introduced by
    bugs; users should never end up with excess shares to begin with.
    """
    from hanson.models.account import UserAccount
    from hanson.models.currency import Points, Shares
    from hanson.models.transaction import create_transaction_annihilate
    from hanson.models.user import User
    from hanson.models.outcome import Outcome

    with begin_transaction(ctx) as tx:
        for user in User.list_all(tx):
            print(f"{user.username}:")
            accounts = list(UserAccount.list_all_for_user(tx, user.id))
            markets = {
                account.market_id
                for account in accounts
                if account.market_id is not None
            }
            balance_by_outcome = {
                account.balance.outcome_id: account.balance
                for account in accounts
                if isinstance(account.balance, Shares)
            }

            for market_id in markets:
                outcomes = Outcome.get_all_by_market(tx, market_id).outcomes
                balances = [
                    balance_by_outcome.get(outcome.id, Shares.zero(outcome.id))
                    for outcome in outcomes
                ]
                min_balance = min(balances)
                print(f"  {market_id=} {min_balance=}", end="")

                if min_balance.is_zero():
                    print()
                else:
                    transaction_id = create_transaction_annihilate(
                        tx,
                        user_id=user.id,
                        market_id=market_id,
                        amount=Points(min_balance.amount),
                    )
                    print(f" -> annihilated in transaction {transaction_id}")

        tx.commit()


if __name__ == "__main__":
    main()
