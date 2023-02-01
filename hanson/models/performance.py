# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import NamedTuple
from decimal import Decimal

from hanson.models.currency import Points
from hanson.database import Transaction


class RealizedGains(NamedTuple):
    # Market for which we measure the user's performance.
    market_id: int

    # User that we measure the performance for.
    user_id: int

    # Amount of points spent on this market to buy shares (non-negative).
    realized_credit: Points

    # Amount of points made on this market from selling shares (non-negative).
    realized_debit: Points

    # Debit minus credit, the net amount made in this market.
    realized_gains: Points

    @staticmethod
    def get_for_market_user(
        tx: Transaction, market_id: int, user_id: int
    ) -> RealizedGains:
        """
        Compute the user's performance in this market.
        """
        credit: Decimal
        debit: Decimal
        credit, debit = tx.execute_fetch_one(
            """
            with credits as (
              select
                amount
              from
                mutations,
                subtransactions,
                accounts as account_user,
                accounts as account_market
              where
                subtransactions.type = 'exchange_create_shares'
                and mutations.subtransaction_id = subtransactions.id
                and mutations.credit_account_id = account_user.id
                and mutations.debit_account_id = account_market.id
                and account_user.owner_user_id = %(user_id)s
                and account_market.owner_market_id = %(market_id)s
            ),
            debits as (
              select
                amount
              from
                mutations,
                subtransactions,
                accounts as account_user,
                accounts as account_market
              where
                subtransactions.type = 'exchange_destroy_shares'
                and mutations.subtransaction_id = subtransactions.id
                and mutations.debit_account_id = account_user.id
                and mutations.credit_account_id = account_market.id
                and account_user.owner_user_id = %(user_id)s
                and account_market.owner_market_id = %(market_id)s
            )
            select
              (select sum(amount) from credits) as credit_amount,
              (select sum(amount) from debits) as debit_amount;
            """,
            {
                "market_id": market_id,
                "user_id": user_id,
            },
        )
        return RealizedGains(
            user_id=user_id,
            market_id=market_id,
            realized_credit=Points(credit),
            realized_debit=Points(debit),
            realized_gains=Points(debit - credit),
        )
