# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, ROUND_DOWN


@dataclass(frozen=True, order=True)
class Amount:
    amount: Decimal

    def is_zero(self) -> bool:
        return self.amount.is_zero()

    # TODO: Can I get Add and Sub required on here?


@dataclass(frozen=True, order=True)
class Points(Amount):
    pass

    @staticmethod
    def zero() -> Points:
        return Points(Decimal("0.00"))

    def __add__(self, other: Points) -> Points:
        return Points(self.amount + other.amount)

    def __sub__(self, other: Points) -> Points:
        return Points(self.amount - other.amount)

    def __neg__(self) -> Points:
        return Points(-self.amount)

    def __floordiv__(self, other: int) -> Points:
        """
        Divide the amount, rounding towards zero to two decimal places.
        """
        return Points(
            (self.amount / other).quantize(Decimal("0.01"), rounding=ROUND_DOWN)
        )

    def __radd__(self, other: int) -> Points:
        # This method is needed to support `sum`, which starts with int 0.
        assert other == 0
        return self

    def __repr__(self) -> str:
        return f"Points({self.amount})"


@dataclass(frozen=True, order=True)
class Shares(Amount):
    outcome_id: int

    @staticmethod
    def zero(outcome_id: int) -> Shares:
        return Shares(Decimal("0.00"), outcome_id)

    def __add__(self, other: Shares) -> Shares:
        assert self.outcome_id == other.outcome_id
        return Shares(self.amount + other.amount, self.outcome_id)

    def __sub__(self, other: Shares) -> Shares:
        assert self.outcome_id == other.outcome_id
        return Shares(self.amount - other.amount, self.outcome_id)

    def __neg__(self) -> Shares:
        return Shares(-self.amount, self.outcome_id)

    def __radd__(self, other: int) -> Shares:
        # This method is needed to support `sum`, which starts with int 0.
        assert other == 0
        return self

    def __repr__(self) -> str:
        return f"Shares[{self.outcome_id}]({self.amount})"
