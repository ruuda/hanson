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
