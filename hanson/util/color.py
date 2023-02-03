# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import NamedTuple


class Color(NamedTuple):
    r: int
    g: int
    b: int

    @staticmethod
    def from_html_hex(hex: str) -> Color:
        """
        Parse a color that consists of six hexadecimal digits and the #,
        e.g. #ff0000.
        """

        assert hex.startswith("#")
        rgb = bytes.fromhex(hex[1:])
        return Color(rgb[0], rgb[1], rgb[2])

    def to_html_hex(self) -> str:
        """
        Format as # and six hexadecimal digits, e.g. #ff0000.
        """
        return f"#{self.r:02x}{self.g:02x}{self.b:02x}"
