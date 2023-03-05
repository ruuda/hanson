#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
The simulator simulates users trading, filling the database with dummy data.
"""

from __future__ import annotations

from random import Random
from typing import Dict, List, NamedTuple, Optional
from datetime import datetime, timezone, timedelta
from dataclasses import dataclass
from decimal import Decimal

from hanson.database import ConnectionPool, Transaction, connect_config
from hanson.models.account import MarketAccount, UserAccount
from hanson.models.color import Color
from hanson.models.config import Config
from hanson.models.currency import Points, Shares
from hanson.models.outcome import Outcome, Outcomes
from hanson.models.market import Market
from hanson.models.probability import ProbabilityDistribution
from hanson.models.user import User
from hanson.models.transaction import (
    create_transaction_income,
    create_transaction_fund_market,
    create_transaction_execute_order,
)
from hanson.routes.market import OrderDetails


def add_users(tx: Transaction) -> List[User]:
    return [
        User.create(tx, "etyrell", "Eldon Tyrell"),
        User.create(tx, "lkowalski", "Leon Kowalski"),
        User.create(tx, "rbatty", "Roy Batty"),
        User.create(tx, "rdeckard", "Rick Deckard"),
    ]


def add_markets(tx: Transaction, author: User, now: datetime) -> List[Market]:
    assert now.tzinfo is not None
    markets = []

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="Does Deckard know?",
        description="Details omitted to avoid spoilers.",
        now=now,
    )
    Outcome.create_discrete(tx, market.id, "Yes", Color.from_html_hex("#68a223"))
    Outcome.create_discrete(tx, market.id, "No", Color.from_html_hex("#a22a17"))
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="Who will be retired first?",
        description=(
            "Deckard has been tasked with retiring four replicants. "
            "Who will he retire first?"
        ),
        now=now,
    )
    Outcome.create_discrete(tx, market.id, "Leon", Color.from_html_hex("#53a5ba"))
    Outcome.create_discrete(tx, market.id, "Pris", Color.from_html_hex("#c0321b"))
    Outcome.create_discrete(tx, market.id, "Roy", Color.from_html_hex("#fff66e"))
    Outcome.create_discrete(tx, market.id, "Zhora", Color.from_html_hex("#8153ba"))
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="When will Roy stop functioning?",
        description=(
            "Roy was activated on January 8, 2016. "
            "But as Tyrell put it, the light that burns twice as bright "
            "burns half as long — and Roy has burned so very, very brightly."
        ),
        now=now,
    )
    n = 10
    c0 = Color.from_html_hex("#006cff")
    c1 = Color.from_html_hex("#ff8100")
    for i in range(n):
        month = ((9 + i) % 12) + 1
        year = 2019 + (9 + i) // 12
        # The colors are interpolated between the endpoints, in CIELUV space.
        c = Color.interpolate_cieluv(c0, c1, i / (n - 1))
        tt = datetime(year, month, 1, 0, 0, 0, tzinfo=timezone.utc)
        Outcome.create_datetime(tx, market.id, f"{year}-{month:02}", c, tt)
    markets.append(market)

    market = Market.create(
        tx,
        author_user_id=author.id,
        title="How many questions will be needed to determine Rachael’s nature?",
        description=(
            "The Voight-Kampff test is a test used to assess whether individuals "
            "are human or replicants by asking them questions and measuring "
            "their physiological response. Typically it takes twenty to thirty "
            "cross-referenced questions to detect a Nexus-6 replicant. "
            "How many questions will Rachael need to answer before the "
            "Voight-Kampff test is conclusive?"
        ),
        now=now,
    )
    n = 5
    for i in range(n):
        # Values are exponentially increasing.
        v: int = 10 * (2**i)
        # The colors are interpolated between the endpoints, in CIELUV space.
        c = Color.interpolate_cieluv(c0, c1, i / (n - 1))
        Outcome.create_float(tx, market.id, str(v), c, float(v))
    markets.append(market)

    return markets


def add_income(
    tx: Transaction, users: List[User], amount: Points, now: datetime
) -> None:
    for user in users:
        create_transaction_income(tx, user.id, amount, now)


def random_probability_distribution(
    rng: Random, outcomes: Outcomes
) -> ProbabilityDistribution:
    """
    Generate a random probability distribution over the given outcomes. By
    default we use a range of -5 to 0 for the logits, which means the ratios
    of the probabilities in the most extreme case will be about 1:150.
    """
    # TODO: For outcomes that are not discrete, this distribution will be quite
    # wonky. We can do a lot better by generating a normal distribution or
    # something that looks like it.
    logits = [rng.uniform(-5.0, 0.0) for _ in range(len(outcomes.outcomes))]
    return ProbabilityDistribution.from_float_logits(logits)


@dataclass
class Sim:
    """
    A Sim is a simulation of a user who is trading in the markets. The database
    itself stores all information about the user and their predictions, but for
    the purpose of the simulation, we also track the user's internal beliefs
    that may not be reflected in the markets.
    """

    user: User

    # For every market (by id), the user's current belief about the
    # probabilities for each outcome.
    beliefs: Dict[int, ProbabilityDistribution]


@dataclass(frozen=False)
class Clock:
    current_time: datetime

    def advance_to(self, t: datetime) -> None:
        assert t.tzinfo is not None
        assert t >= self.current_time
        self.current_time = t

    def tick(self, dt: timedelta) -> None:
        self.current_time = self.current_time + dt


class Simulator(NamedTuple):
    rng: Random
    conn: ConnectionPool
    clock: Clock
    users: List[User]
    markets: List[Market]
    sims: List[Sim]

    @staticmethod
    def new(conn: ConnectionPool, t0: datetime, seed: int = 42) -> Simulator:
        rng = Random(seed)

        with conn.begin() as tx:
            users = add_users(tx)

            add_income(tx, users, Points(Decimal("10.00")), t0)

            # For simplicity, the first user is going to be the author of all markets.
            etyrell = users[0]
            markets = add_markets(tx, etyrell, t0)

            # We give this user some additional income to pay for funding the markets.
            create_transaction_income(
                tx, etyrell.id, Points(Decimal("5.0") * len(markets)), t0
            )
            for market in markets:
                create_transaction_fund_market(
                    tx, etyrell.id, market.id, Points(Decimal("5.00")), t0
                )

            market_outcomes = [
                Outcome.get_all_by_market(tx, market.id) for market in markets
            ]

            tx.commit()

        sims = [
            Sim(
                user=user,
                beliefs={
                    market.id: random_probability_distribution(rng, outcomes)
                    for market, outcomes in zip(markets, market_outcomes)
                },
            )
            for user in users
        ]

        clock = Clock(t0)
        return Simulator(rng, conn, clock, users, markets, sims)

    def sim_update_beliefs(self, tx: Transaction, sim: Sim) -> None:
        """
        Evolve the sim's beliefs by integrating various sources:
        * The sim's own belief should become more extreme, so eventually it
          starts expecting one particular outcome over all others.
        * Add some randomness, to simulate the sim learning new things,
          responding to events, or just changes in mood or whatever.
        * Converge a little towards the market price. The sim thinks it knows
          better, but the market can't be completely wrong.
        """
        for market in self.markets:
            outcomes = Outcome.get_all_by_market(tx, market.id)
            pool_accounts = [
                MarketAccount.expect_pool_account(tx, market.id, outcome.id)
                for outcome in outcomes.outcomes
            ]
            pool_pd = ProbabilityDistribution.from_pool_balances(
                [pool_account.balance for pool_account in pool_accounts]
            )
            self_pd = sim.beliefs[market.id]

            # Then step 10% towards the market prediction.
            self_pd = self_pd.interpolate(pool_pd, Decimal("0.10"))

            # Then step 5% in a random direction, to simulate evolving beliefs.
            random_pd = random_probability_distribution(self.rng, outcomes)
            self_pd = self_pd.interpolate(random_pd, Decimal("0.05"))

            # If we are not already quite certain of our beliefs, then now we
            # become a bit more certain of our beliefs. If we are very certain,
            # then we become a bit less certain. Targeting a particular entropy
            # range is mostly to keep the graphs interesting, so the market does
            # not converge on a single outcome.
            entropy = self_pd.entropy()
            if entropy > 0.9:
                self_pd = ProbabilityDistribution.from_float_logits(
                    [float(x) * 1.1 for x in self_pd.logits]
                )
            elif entropy < 0.3:
                self_pd = ProbabilityDistribution.from_float_logits(
                    [float(x) * 0.9 for x in self_pd.logits]
                )

            print(f"Updating user {sim.user.id} for market {market.id}")
            print(
                f"  Before: {sim.beliefs[market.id]} ~ {sim.beliefs[market.id].entropy():.3f}"
            )
            print(f"  After:  {self_pd} ~ {self_pd.entropy():.3f}")
            sim.beliefs[market.id] = self_pd

    def sim_trade_once(self, tx: Transaction, sim: Sim, budget: Points) -> Points:
        """
        Look at all markets, then given the budget, decide which market we could
        get the most ROI in, and trade in that market. Returns the number of
        points spent.
        """
        assert budget > Points.zero()

        best_roi = 1.0
        order: Optional[OrderDetails] = None

        for market in self.markets:
            outcomes = Outcome.get_all_by_market(tx, market.id)
            pool_accounts = [
                MarketAccount.expect_pool_account(tx, market.id, outcome.id)
                for outcome in outcomes.outcomes
            ]
            pool_pd = ProbabilityDistribution.from_pool_balances(
                [pool_account.balance for pool_account in pool_accounts]
            )
            self_pd = sim.beliefs[market.id]
            expected_roi = 0.0
            for p_pool, p_self in zip(pool_pd.ps(), self_pd.ps()):
                outcome_roi = p_self / p_pool
                expected_roi += outcome_roi * p_self

            print(f"Market {market.id}:")
            print(f"  expected roi: {expected_roi:.2f}")
            print(f"  pool pd: {pool_pd}")
            print(f"  self pd: {self_pd}")

            if expected_roi > best_roi:
                best_roi = expected_roi
                order = OrderDetails.for_target_distribution(
                    tx,
                    user_id=sim.user.id,
                    market=market,
                    outcomes=outcomes,
                    pd_target=self_pd,
                    max_spend=budget,
                )

        if order is not None:
            print(f"Will trade in market {order.market.id}:")
            print(f"  pd_after:     {order.pd_after}")
            print(f"  costs_shares: {order.costs_shares}")
            print(f"  cost_points:  {order.cost_points}")

            tx_id = create_transaction_execute_order(
                tx,
                debit_user_id=sim.user.id,
                credit_market_id=order.market.id,
                cost=order.cost_points,
                amounts=[
                    Shares(cost, outcome_id=outcome.id)
                    for outcome, cost in zip(
                        order.outcomes.outcomes, order.costs_shares
                    )
                ],
                now=self.clock.current_time,
            )
            print(f"  order tx_id:  {tx_id}")
            return order.cost_points

        return Points.zero()

    def sim_update(self, sim: Sim) -> None:
        with self.conn.begin() as tx:
            self.sim_update_beliefs(tx, sim)

            user_points_account = UserAccount.expect_points_account(tx, sim.user.id)
            # Per step, we aim to spend at most 1/3 of our balance. Per trade,
            # we aim to spend at most half of the remaining budget.
            budget = user_points_account.balance // 3
            print(f"\nBudget for user {sim.user.id}: {budget}")

            while budget > Points.zero():
                points_spent = self.sim_trade_once(tx, sim, budget // 2)
                budget = budget - points_spent
                if points_spent < Points(Decimal("0.50")):
                    break

                # Pretend we spent five minutes pondering the trade.
                self.clock.tick(timedelta(minutes=5))

            tx.commit()


def main() -> None:
    # Backdate market creation to a fixed time in the past, so we can run
    # the simulation for a ~year and also see the graphs in the webinterface.
    t0 = datetime(2022, 3, 1, 15, 0, 0, tzinfo=timezone.utc)
    t1 = datetime(2023, 3, 1, 15, 0, 0, tzinfo=timezone.utc)

    config = Config.load_from_toml_file("config.toml")
    conn = connect_config(config)

    simulator = Simulator.new(conn, t0)
    for sim in simulator.sims:
        for market_id, pd in sim.beliefs.items():
            print(f"user={sim.user.id} market={market_id} => {pd}")

    while simulator.clock.current_time < t1:
        for sim in simulator.sims:
            simulator.sim_update(sim)

        simulator.clock.tick(timedelta(days=1))
        with simulator.conn.begin() as tx:
            add_income(
                tx,
                simulator.users,
                Points(Decimal("1.00")),
                simulator.clock.current_time,
            )
            tx.commit()


if __name__ == "__main__":
    main()
