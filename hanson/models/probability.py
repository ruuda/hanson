# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

import math

from decimal import Decimal
from typing import List, NamedTuple

from hanson.models.currency import Shares


class ProbabilityDistribution(NamedTuple):
    """
    A probability distribution over a discrete set of outcomes.
    """

    # Stores log(p) for every probability p.
    logits: List[Decimal]

    @staticmethod
    def from_pool_balances(balances: List[Shares]) -> ProbabilityDistribution:
        logits = [-x.amount for x in balances]
        # If we stored just the balances, then if we later apply exp, the result
        # would not be normalized, so we would need to divide by the sum of the
        # exps at that point. But taking the log of a quotient is taking the
        # difference of the logs, so we can already subtract that correction
        # factor here. Also, sort the numbers before summing them to improve
        # precision by summing numbers of similar size first.
        offset = Decimal.from_float(math.log(sum(sorted(math.exp(x) for x in logits))))
        return ProbabilityDistribution([logit - offset for logit in logits])

    @staticmethod
    def from_float_logits(float_logits: List[float]) -> ProbabilityDistribution:
        # See also `from_pool_balances`, this is just that without the negation.
        logits = [Decimal.from_float(x) for x in float_logits]
        offset = Decimal.from_float(math.log(sum(sorted(math.exp(x) for x in logits))))
        return ProbabilityDistribution([logit - offset for logit in logits])

    @staticmethod
    def from_probabilities(ps: List[float]) -> ProbabilityDistribution:
        """
        Construct a probability distribution given the probabilities. They do
        not need to be normalized, we normalize them either way.
        """
        # If we assigned zero probability to anything, the log below would fail.
        # So limit extreme probabilities to 0.01%.
        # TODO: Add test case for this.
        ps_nonzero = [p if p > 0.0 else 0.0001 for p in ps]
        offset = Decimal.from_float(math.log(sum(ps_nonzero)))
        return ProbabilityDistribution(
            [Decimal.from_float(math.log(p)) - offset for p in ps_nonzero]
        )

    def ps(self) -> List[float]:
        """
        Return the probabilites for every outcome.
        """
        numers = [math.exp(x) for x in self.logits]
        denom = sum(numers)
        return [x / denom for x in numers]

    def entropy(self) -> float:
        """
        Return the entropy of the distribution.
        """
        return -sum(p * float(logit) for p, logit in zip(self.ps(), self.logits))

    def interpolate(
        self, other: ProbabilityDistribution, t: Decimal
    ) -> ProbabilityDistribution:
        """
        Return `self` for t=0, `other` for t=1, and a distribution in between
        for a t in between. This doesn't do a linear interpolation on the
        probabilities; instead it performs a linear interpolation on the log
        probabilities.
        """
        assert len(self.logits) == len(other.logits)
        assert 0 <= t <= 1

        return ProbabilityDistribution(
            [(1 - t) * x + t * y for x, y in zip(self.logits, other.logits)]
        )

    def cost_for_update(self, other: ProbabilityDistribution) -> List[Decimal]:
        """
        Return the change required in each outcome share pool balance, to update
        from `self` to `other`. This assumes that the reward for outcome i if it
        is the true outcome, is -log(ps[i]).

        The units of the returned values are in shares, but this method does not
        wrap the result in outcome shares, because it doesn't know the outcome
        ids.
        """
        assert len(self.logits) == len(other.logits)
        return [x - y for x, y in zip(self.logits, other.logits)]

    def most_likely_index(self) -> int:
        """
        Return the index of the outcome with the greatest probability.
        """
        max_p, index = max(zip(self.logits, range(len(self.logits))))
        return index

    def __repr__(self) -> str:
        return "[" + " ".join(f"{x:.4f}" for x in self.ps()) + "]"
