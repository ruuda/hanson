# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import Iterable, List, NamedTuple, Optional, Union
from datetime import datetime
from dataclasses import dataclass

from hanson.models.color import Color
from hanson.database import Transaction
from hanson.models.probability import ProbabilityDistribution


@dataclass(frozen=True)
class Outcome:
    id: int
    market_id: int
    name: str
    color: Color

    @staticmethod
    def create_discrete(
        tx: Transaction,
        market_id: int,
        name: str,
        color: Color,
    ) -> OutcomeDiscrete:
        outcome_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO outcomes (market_id, name, color)
            VALUES (%s, %s, %s)
            RETURNING id;
            """,
            (market_id, name, color.to_html_hex()),
        )
        return OutcomeDiscrete(outcome_id, market_id, name, color)

    @staticmethod
    def create_float(
        tx: Transaction,
        market_id: int,
        name: str,
        color: Color,
        value: float,
    ) -> OutcomeFloat:
        outcome_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO outcomes (market_id, name, color, value_float)
            VALUES (%s, %s, %s, %s)
            RETURNING id;
            """,
            (market_id, name, color.to_html_hex(), value),
        )
        return OutcomeFloat(outcome_id, market_id, name, color, value)

    @staticmethod
    def create_datetime(
        tx: Transaction,
        market_id: int,
        name: str,
        color: Color,
        value: datetime,
    ) -> OutcomeDatetime:
        assert value.tzinfo is not None
        outcome_id: int = tx.execute_fetch_scalar(
            """
            INSERT INTO outcomes (market_id, name, color, value_datetime)
            VALUES (%s, %s, %s, %s)
            RETURNING id;
            """,
            (market_id, name, color.to_html_hex(), value),
        )
        return OutcomeDatetime(outcome_id, market_id, name, color, value)

    @staticmethod
    def get_all_by_market_unchecked(
        tx: Transaction, market_id: int
    ) -> Iterable[Outcome]:
        """
        Iterate all outcomes for a given market, but don't check that the type
        is consistent for all outcomes.
        """
        id: int
        name: str
        color_hex: str
        value_float: Optional[float]
        value_datetime: Optional[datetime]
        for id, name, color_hex, value_float, value_datetime in tx.execute_fetch_all(
            """
            SELECT
              id,
              name,
              color,
              value_float,
              value_datetime
            FROM
              outcomes
            WHERE
              market_id = %s
            ORDER BY
              id
            """,
            (market_id,),
        ):
            color = Color.from_html_hex(color_hex)

            if value_float is not None:
                yield OutcomeFloat(id, market_id, name, color, value_float)
            elif value_datetime is not None:
                yield OutcomeDatetime(id, market_id, name, color, value_datetime)
            else:
                yield OutcomeDiscrete(id, market_id, name, color)

    @staticmethod
    def get_all_by_market(tx: Transaction, market_id: int) -> Outcomes:
        outcomes = list(Outcome.get_all_by_market_unchecked(tx, market_id))
        assert len(outcomes) >= 2, "A market must have at least two outcomes."

        # Verify that all elements are of the same type, and then put things in
        # a more strongly typed container that indicates that. Unfortunately
        # Mypy cannot check this for us.

        if isinstance(outcomes[0], OutcomeFloat):
            assert all(isinstance(oc, OutcomeFloat) for oc in outcomes)
            return OutcomesFloat(outcomes)  # type: ignore[arg-type]

        elif isinstance(outcomes[0], OutcomeDatetime):
            assert all(isinstance(oc, OutcomeDatetime) for oc in outcomes)
            return OutcomesDatetime(outcomes)  # type: ignore[arg-type]

        else:
            assert all(isinstance(oc, OutcomeDiscrete) for oc in outcomes)
            return OutcomesDiscrete(outcomes)  # type: ignore[arg-type]

    def get_sanitized_color(self) -> str:
        """
        Return a toned-down color that has good enough contrast to be usable in
        the UI. Returns the html-formatted hex color.
        """
        return self.color.clamp_for_ui().to_html_hex()


@dataclass(frozen=True)
class OutcomeDiscrete(Outcome):
    value: None = None


@dataclass(frozen=True)
class OutcomeFloat(Outcome):
    value: float


@dataclass(frozen=True)
class OutcomeDatetime(Outcome):
    value: datetime


class OutcomesDiscrete(NamedTuple):
    outcomes: List[OutcomeDiscrete]


class OutcomesFloat(NamedTuple):
    outcomes: List[OutcomeFloat]

    def get_quantiles(
        self, quantiles: List[float], pd: ProbabilityDistribution
    ) -> List[float]:
        """
        Given a probability distribution for these outcomes, return the desired
        quantiles.
        """
        values = [oc.value for oc in self.outcomes]
        assert (
            list(sorted(values)) == values
        ), "Outcomes must be sorted by ascending value."
        assert list(sorted(quantiles)) == quantiles, "Quantiles must be sorted."
        assert len(values) > 1, "Must have at least two outcomes."

        p0 = 0.0
        x0 = self.outcomes[0].value
        xp_iter = iter(zip(values, pd.ps()))
        q_iter = iter(quantiles)
        x1, p1 = next(xp_iter)
        q = next(q_iter)

        result: List[float] = []

        try:
            while True:
                if p1 >= q:
                    # The desired quantile lies inside this segment.
                    t = (q - p0) / (p1 - p0)
                    x = x0 * (1.0 - t) + x1 * t
                    result.append(x)
                    q = next(q_iter)
                    continue

                x0, p0 = x1, p1
                x1, p = next(xp_iter)
                p1 = p0 + p

        except StopIteration:
            # If we exhaust the quantile iterator, then we are done. If we
            # exhaust the probability iterator, that should not happen for
            # quantiles in 0..1, but it can happen for invalid inputs past 1,
            # in that case we repeat the upper bound.
            while len(result) < len(quantiles):
                result.append(x1)

        return result


class OutcomesDatetime(NamedTuple):
    outcomes: List[OutcomeDatetime]


Outcomes = Union[OutcomesDiscrete, OutcomesFloat, OutcomesDatetime]
