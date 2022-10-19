# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from datetime import datetime, timezone
from typing import Iterable

from hanson.models.history import ProbabilityHistory
from hanson.models.outcome import Outcome


def tick_label(bin_size_secs: int, t: datetime) -> str:
    assert t.tzinfo is not None
    t = t.astimezone(timezone.utc)

    if bin_size_secs <= 3600 * 3:
        return t.strftime("%H:%M")
    elif bin_size_secs < 3600 * 24 * 3:
        return t.strftime("%b %d %H:%M")
    else:
        return t.strftime("%Y-%m-%d")


def render_graph(
    *,
    ps_history: ProbabilityHistory,
    outcomes: Iterable[Outcome],
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
    graph_height = axis_height + 2.0

    result = []
    result.append(
        f"""<svg version="1.1" xmlns="xmlns="http://www.w3.org/2000/svg" width="100%" height="18em" viewbox="0 0 {num_ticks} {graph_height:.3f}">"""
    )

    prev_x = 0
    prev_pd = None

    current_tick = end_tick
    current_time = end_time
    current_elem = len(ps_history.history) - 1
    n_until_tick = 0

    while current_tick > start_tick:
        current_time, current_ps = ps_history.history[current_elem]

        if current_time.timestamp() // bin_size_secs > current_tick:
            current_elem -= 1

            if current_elem < 0:
                break

            current_time, current_ps = ps_history.history[current_elem]

        x = current_tick - start_tick
        start_y = 0.0
        bar_width = 0.8

        for outcome, p in zip(outcomes, current_ps.ps()):
            height = p * axis_height
            result.append(
                f'<rect x="{x - 0.5 - bar_width / 2:.2f}" y="{start_y:.3f}" '
                f'width="{bar_width:.2f}" height="{height:.3f}" '
                f'fill="{outcome.color}"></rect>'
            )
            start_y += p * axis_height

        if n_until_tick == 0:
            label = tick_label(
                bin_size_secs=bin_size_secs,
                t=datetime.fromtimestamp(current_tick * bin_size_secs, tz=timezone.utc),
            )
            result.append(
                f'<circle cx="{x - 0.5:.2f}" '
                f'cy="{axis_height + 0.4:.2f}" '
                f'r="0.1"></circle>'
                f'<text x="{x - 0.5:.2f}" '
                f'y="{axis_height + 1.2:.2f}" '
                f'text-anchor="middle">{label}</text>'
            )
            n_until_tick = 8
        else:
            n_until_tick -= 1

        current_tick -= 1

    result.append("</svg>")
    return "\n".join(result)
