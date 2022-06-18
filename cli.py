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

from hanson.database import connect_default
from hanson.models.user import User
from hanson.models.transaction import create_transaction_income
from hanson.models.currency import Points


@click.group()
def main() -> None:
    pass


@main.command()
@click.argument("points_per_user", required=True, type=Decimal)
def airdrop(points_per_user: Decimal) -> None:
    amount = Points(points_per_user)

    conn = connect_default()
    with conn.begin() as tx:
        for user in User.list_all(tx):
            transaction_id = create_transaction_income(
                tx,
                user_id=user.id,
                amount=amount,
            )
            print(f"{user.username}: +{amount.amount} points")

        tx.commit()


if __name__ == "__main__":
    main()
