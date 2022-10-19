# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from datetime import datetime, timedelta, timezone
from typing import Iterable, NamedTuple, Optional

from hanson.models.history import ProbabilityHistory
from hanson.models.outcome import Outcome


class TickFormat(NamedTuple):
    timetuple_index: str
    timetuple_multiple: int
    format: str

    @staticmethod
    def time_axis_ticks(start_time: datetime, end_time: datetime) -> TickFormat:
        """
        Return suitable ticks for a given time range.
        """
        assert start_time.tzinfo is not None
        assert end_time.tzinfo is not None

        y0, m0, d0, hh0, mm0, *_ = start_time.utctimetuple()
        y1, m1, d1, hh1, mm1, *_ = end_time.utctimetuple()
        duration = end_time - start_time

        format_parts = ["", "", "%H:%M"]
        index = "hours"
        multiple = 1

        if d1 > d0 or m1 > m0:
            format_parts[1] = "%b %d"

        if y1 > y0:
            format_parts[0] = "%Y"

        if duration > timedelta(hours=12):
            multiple = 8

        if duration > timedelta(days=7):
            index = "days"
            format_parts[2] = ""
            multiple = 2

        if duration > timedelta(days=14):
            multiple = 5

        if duration > timedelta(days=30):
            multiple = 15

        if duration > timedelta(days=60):
            index = "months"
            multiple = 1

        return TickFormat(
            index,
            multiple,
            " ".join(p for p in format_parts if p != ""),
        )

    def get_tick(self, t: datetime) -> Optional[str]:
        """
        Format a tick, if we should tick at this particular time.
        """
        assert t.tzinfo is not None

        y, m, d, h, *_ = t.utctimetuple()

        if self.timetuple_index == "hours":
            if h % self.timetuple_multiple > 0:
                return None

        elif self.timetuple_index == "days":
            if (d - 1) % self.timetuple_multiple > 0:
                return None

        elif self.timetuple_index == "months":
            if d > 1:
                return None

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

    prev_x = 0
    prev_pd = None

    current_tick = end_tick
    current_time = end_time
    current_elem = len(ps_history.history) - 1

    tick_format = TickFormat.time_axis_ticks(start_time, end_time)

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

        t = datetime.fromtimestamp(current_tick * bin_size_secs, tz=timezone.utc)
        tick_label = tick_format.get_tick(t)
        if tick_label is not None:
            result.append(
                f'<circle cx="{x - 0.5:.2f}" '
                f'cy="{axis_height + 0.4:.2f}" '
                f'r="0.1"></circle>'
                f'<text x="{x - 0.5:.2f}" '
                f'y="{axis_height + 1.2:.2f}" '
                f'text-anchor="middle">{tick_label}</text>'
            )

        current_tick -= 1

    result.append("</svg>")
    return "\n".join(result)
