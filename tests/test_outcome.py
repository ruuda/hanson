from typing import List

from hanson.models.probability import ProbabilityDistribution
from hanson.models.outcome import OutcomesFloat, OutcomeFloat
from hanson.models.color import Color


def sum_abs_diff(xs: List[float], ys: List[float]) -> float:
    return sum(abs(x - y) for x, y in zip(xs, ys))


def test_outcomes_float_get_quantiles_approximates_uniform() -> None:
    """
    Start with a uniform distribution (50% x=0, 50% x=1), and probe the
    quantiles to ensure that they are as we would expect from a uniform
    distribution.
    """
    color = Color.from_html_hex("#000000")
    pd = ProbabilityDistribution.from_probabilities([0.5, 0.5])
    os = OutcomesFloat(
        [
            OutcomeFloat(id=1, market_id=1, name="x=0.0", color=color, value=0.0),
            OutcomeFloat(id=2, market_id=1, name="x=1.0", color=color, value=1.0),
        ]
    )

    assert sum_abs_diff(os.get_quantiles([0.1], pd), [0.1]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.5], pd), [0.5]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.9], pd), [0.9]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.25, 0.75], pd), [0.25, 0.75]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.0, 1.0], pd), [0.0, 1.0]) < 1e-6


def test_outcomes_float_get_quantiles() -> None:
    color = Color.from_html_hex("#000000")
    pd = ProbabilityDistribution.from_probabilities([0.1, 0.2, 0.3, 0.4])
    os = OutcomesFloat(
        [
            OutcomeFloat(id=1, market_id=1, name="x=1.0", color=color, value=1.0),
            OutcomeFloat(id=2, market_id=1, name="x=2.0", color=color, value=2.0),
            OutcomeFloat(id=3, market_id=1, name="x=5.0", color=color, value=5.0),
            OutcomeFloat(id=4, market_id=1, name="x=7.0", color=color, value=7.0),
        ]
    )

    # When we ask for the cumulative probabilities that align with our data
    # points, we should get the midpoints of our data.
    assert sum_abs_diff(os.get_quantiles([0.1], pd), [1.5]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.3], pd), [3.5]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.6], pd), [6.0]) < 1e-6

    # The same should hold regardless of whether we asked for the quantiles one
    # by one or all in one batch.
    assert sum_abs_diff(os.get_quantiles([0.1, 0.3, 0.6], pd), [1.5, 3.5, 6.0]) < 1e-6

    # At the extreme quantiles, we should get the extrema.
    assert sum_abs_diff(os.get_quantiles([0.0, 1.0], pd), [1.0, 7.0]) < 1e-6

    # For the first and last bucket, in the center of their bucket quantile-wise
    # we should be in between the value and the midpoint.
    assert sum_abs_diff(os.get_quantiles([0.05, 0.8], pd), [1.25, 6.5]) < 1e-6

    # For the middle buckets, in the center of their bucket quantile-wise,
    # we should be in between the surrounding midpoints -- which does not have
    # to be the value itself, because the buckets may not be spaced evenly.
    assert sum_abs_diff(os.get_quantiles([0.20, 0.45], pd), [2.5, 4.75]) < 1e-6
