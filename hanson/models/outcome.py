from __future__ import annotations

from typing import Iterable, List, NamedTuple, Optional, Tuple, Union
from datetime import datetime
from dataclasses import dataclass

from hanson.database import Transaction


@dataclass(frozen=True)
class Outcome:
    id: int
    market_id: int
    name: str
    color: str

    @staticmethod
    def create_discrete(
        tx: Transaction,
        market_id: int,
        name: str,
        color: str,
    ) -> OutcomeDiscrete:
        with tx.cursor() as cur:
            cur.execute(
                """
                INSERT INTO "outcome" (market_id, name, color)
                VALUES (%s, %s, %s)
                RETURNING id;
                """,
                (market_id, name, color),
            )
            outcome_id = cur.fetchone()[0]
            return OutcomeDiscrete(outcome_id, market_id, name, color)

    @staticmethod
    def create_float(
        tx: Transaction,
        market_id: int,
        name: str,
        color: str,
        value: float,
    ) -> OutcomeFloat:
        with tx.cursor() as cur:
            cur.execute(
                """
                INSERT INTO "outcome" (market_id, name, color, value_float)
                VALUES (%s, %s, %s, %s)
                RETURNING id;
                """,
                (market_id, name, color, value),
            )
            outcome_id = cur.fetchone()[0]
            return OutcomeFloat(outcome_id, market_id, name, color, value)

    @staticmethod
    def create_datetime(
        tx: Transaction,
        market_id: int,
        name: str,
        color: str,
        value: datetime,
    ) -> OutcomeDatetime:
        assert value.tzinfo is not None
        with tx.cursor() as cur:
            cur.execute(
                """
                INSERT INTO "outcome" (market_id, name, color, value_datetime)
                VALUES (%s, %s, %s, %s)
                RETURNING id;
                """,
                (market_id, name, color, value),
            )
            outcome_id = cur.fetchone()[0]
            return OutcomeDatetime(outcome_id, market_id, name, color, value)

    @staticmethod
    def get_all_by_market_unchecked(
        tx: Transaction, market_id: int
    ) -> Iterable[Outcome]:
        """
        Iterate all outcomes for a given market, but don't check that the type
        is consistent for all outcomes.
        """
        with tx.cursor() as cur:
            cur.execute(
                """
                SELECT
                  id,
                  name,
                  color,
                  value_float,
                  value_datetime
                FROM
                  "outcome"
                WHERE
                  market_id = %s
                """,
                (market_id,),
            )
            while True:
                row: Optional[
                    Tuple[int, str, str, Optional[float], Optional[datetime]]
                ] = cur.fetchone()
                if row is None:
                    break

                id, name, color, value_float, value_datetime = row
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


class OutcomesDatetime(NamedTuple):
    outcomes: List[OutcomeDatetime]


Outcomes = Union[OutcomesDiscrete, OutcomesFloat, OutcomesDatetime]
