from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal


@dataclass(frozen=True)
class Amount:
    amount: Decimal


@dataclass(frozen=True)
class Points(Amount):
    pass

    @staticmethod
    def zero() -> Points:
        return Points(Decimal('0.00'))

    def __add__(self, other: Points) -> Points:
        return Points(self.amount + other.amount)

    def __radd__(self, other: int) -> Points:
        # This method is needed to support `sum`, which starts with int 0.
        assert other == 0
        return self


@dataclass(frozen=True)
class OutcomeShares(Amount):
    outcome_id: int

    @staticmethod
    def zero(outcome_id: int) -> OutcomeShares:
        return OutcomeShares(Decimal('0.00'), outcome_id)

    def __add__(self, other: OutcomeShares) -> OutcomeShares:
        assert self.outcome_id == other.outcome_id
        return OutcomeShares(self.amount + other.amount, self.outcome_id)

    def __radd__(self, other: int) -> OutcomeShares:
        # This method is needed to support `sum`, which starts with int 0.
        assert other == 0
        return self
