# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from datetime import datetime, timedelta, timezone
from typing import Dict, Iterable, List, NamedTuple, Optional

from hanson.models.history import ProbabilityHistory
from hanson.models.outcome import Outcome


class TickFormat(NamedTuple):
    format: str
    origin: datetime

    @staticmethod
    def time_axis_ticks(x_coords: List[datetime]) -> TickFormat:
        """
        Return suitable ticks for a given time range.
        """
        assert all(t.tzinfo is not None for t in x_coords)

        # For the formatting of the labels, choose enough resolution to
        # make all ticks unambiguous, i.e. avoid duplicate labels in the range.
        ys, mds, hhmms = set(), set(), set()

        for t in x_coords:
            y, m, d, hh, mm, *_ = t.utctimetuple()
            ys.add(y)
            mds.add((m, d))
            hhmms.add((hh, mm))

        format_parts = []

        if len(ys) > 1:
            format_parts.append("%Y")

        if len(mds) > 1:
            format_parts.append("%b %d")

        if len(hhmms) > 1:
            format_parts.append("%H:%M")

        # Then aside from the labels, we have to choose where to place the
        # ticks. For now we space them every 4 bars, which means the only thing
        # we get to choose is the origin, the location of one tick. We choose
        # to align it based on the duration of the interval to either 10 minutes,
        # hours, days, the first day of the month, or the first day of the year.
        start_time = min(x_coords)
        end_time = max(x_coords)
        duration = end_time - start_time
        end_y, end_m, end_d, end_hh, end_mm, *_ = end_time.utctimetuple()

        origin = datetime(
            end_y, end_m, end_d, end_hh, end_mm // 10 * 10, 0, tzinfo=timezone.utc
        )

        if duration > timedelta(hours=4):
            origin = datetime(end_y, end_m, end_d, end_hh, 0, 0, tzinfo=timezone.utc)

        if duration > timedelta(hours=11):
            origin = datetime(end_y, end_m, end_d, 0, 0, 0, tzinfo=timezone.utc)

        if duration > timedelta(days=13):
            origin = datetime(end_y, end_m, 1, 0, 0, 0, tzinfo=timezone.utc)

        if duration > timedelta(days=90):
            origin = datetime(end_y, 1, 1, 0, 0, 0, tzinfo=timezone.utc)

        return TickFormat(" ".join(p for p in format_parts if p != ""), origin)

    def get_label(self, t: datetime) -> str:
        """
        Format a tick, if we should tick at this particular time.
        """
        assert t.tzinfo is not None
        return t.strftime(self.format)


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

    current_tick = end_tick
    current_elem = len(ps_history.history) - 1

    time_ticks = [t for t, ps in ps_history.history]
    tick_format = TickFormat.time_axis_ticks(time_ticks)
    origin_tick = tick_format.origin.timestamp() // bin_size_secs

    polylines: Dict[int, List[str]] = {}

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
                f'fill="{outcome.get_sanitized_color()}" opacity="0.3"></rect>'
            )
            start_y += p * axis_height

            polyline = polylines.setdefault(outcome.id, [])
            polyline.append(f"{x:.2f},{start_y - height:.3f}")


        t = datetime.fromtimestamp(current_tick * bin_size_secs, tz=timezone.utc)
        if (current_tick - origin_tick) % 4 == 0:
            tick_label = tick_format.get_label(t)
            result.append(
                f'<circle cx="{x - 0.5:.2f}" '
                f'cy="{axis_height + 0.4:.2f}" '
                f'r="0.1"></circle>'
                f'<text x="{x - 0.5:.2f}" '
                f'y="{axis_height + 1.2:.2f}" '
                f'text-anchor="middle">{tick_label}</text>'
            )

        current_tick -= 1

    for outcome in outcomes:
        points = " ".join(polylines[outcome.id])
        color = outcome.get_sanitized_color()
        result.append(
            f'<polyline points="{points}" '
            f'fill="none" stroke="{color}" '
            'stroke-width="0.15" '
            'stroke-linejoin="round" '
            '/>'
        )

    result.append("</svg>")
    return "\n".join(result)
