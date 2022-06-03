from datetime import datetime, timedelta

from typing import List

from hanson.models.probability import ProbabilityDistribution
from hanson.models.history import ProbabilityHistory
from hanson.models.outcome import Outcome

def render_graph(
    *,
    ps_history: ProbabilityHistory,
    outcomes: List[Outcome],
    start_time: datetime,
    end_time: datetime,
) -> str:
    assert start_time.tzinfo is not None
    assert end_time.tzinfo is not None

    bin_size_secs = int(ps_history.bin_size.total_seconds())
    start_tick = start_time.timestamp() // bin_size_secs
    end_tick = (end_time + ps_history.bin_size).timestamp() // bin_size_secs
    num_ticks = end_tick - start_tick

    aspect_ratio = 21 / 9
    axis_height = num_ticks / aspect_ratio

    result = []
    result.append(
        f"""<svg version="1.1" xmlns="xmlns="http://www.w3.org/2000/svg" width="100%" height="16em" viewbox="0 0 {num_ticks} {axis_height:.3f}">"""
    )

    prev_x = 0
    prev_pd = None

    current_tick = end_tick
    current_time = end_time
    current_elem = len(ps_history.history) - 1

    while current_tick > start_tick:
        current_time, current_ps = ps_history.history[current_elem]

        if current_time.timestamp() // bin_size_secs > current_tick:
            current_elem -= 1

            if current_elem < 0:
                break

            current_time, current_ps = ps_history.history[current_elem]

        x = current_tick - start_tick
        start_y = 0

        for outcome, p in zip(outcomes, current_ps.ps()):
            height = p * axis_height
            result.append(
                f'<rect x="{x - 0.5 - 0.4:.1f}" y="{start_y:.3f}" '
                f'width="0.8" height="{height:.3f}" '
                f'fill="{outcome.color}"></rect>'
            )
            start_y += p * axis_height

        current_tick -= 1

    result.append("</svg>")
    return "\n".join(result)
