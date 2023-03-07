from typing import List

from hanson.models.probability import ProbabilityDistribution
from hanson.models.outcome import OutcomesFloat, OutcomeFloat
from hanson.models.color import Color


def sum_abs_diff(xs: List[float], ys: List[float]) -> float:
    return sum(abs(x - y) for x, y in zip(xs, ys))


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
    # points, we should get those data points back.
    assert sum_abs_diff(os.get_quantiles([0.1], pd), [1.0]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.3], pd), [2.0]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.6], pd), [5.0]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([1.0], pd), [7.0]) < 1e-6

    # The same should hold regardless of whether we asked for the quantiles one
    # by one or all in one batch.
    assert (
        sum_abs_diff(os.get_quantiles([0.1, 0.3, 0.6, 1.0], pd), [1.0, 2.0, 5.0, 7.0])
        < 1e-6
    )

    # If we ask for a cumulative probability in between two data points, we
    # should get the value in between.
    assert sum_abs_diff(os.get_quantiles([0.20], pd), [1.5]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.45], pd), [3.5]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([0.7, 0.8, 0.9], pd), [5.5, 6.0, 6.5]) < 1e-6

    # If we ask for a cumulative probability outside the bounds, then we
    # should get one of the extreme values.
    assert sum_abs_diff(os.get_quantiles([0.05, 0.09], pd), [1.0, 1.0]) < 1e-6
    assert sum_abs_diff(os.get_quantiles([1.05, 1.09], pd), [7.0, 7.0]) < 1e-6
