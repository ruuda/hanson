# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import NamedTuple, Tuple

import colorsys
import math

Vec3 = Tuple[float, float, float]


def srgb_to_linear(v: float) -> float:
    """
    Map any rgb component of sRGB to linear RGB.
    Input and output are in the range [0, 1].
    """
    if v < 0.04045:
        return v / 12.92
    else:
        return ((v + 0.055) / 1.055) ** 2.4


def linear_to_srgb(v: float) -> float:
    """
    Inverse of `srgb_to_linear`.
    """
    if v < 0.0031308:
        return v * 12.92
    else:
        return (v ** (1 / 2.4)) * 1.055 - 0.055


def linear_rgb_to_xyz(r: float, g: float, b: float) -> Vec3:
    """
    Convert linear RGB to CIE XYZ.
    """
    x = 0.4124 * r + 0.3576 * g + 0.1805 * b
    y = 0.2126 * r + 0.7152 * g + 0.0722 * b
    z = 0.0193 * r + 0.1192 * g + 0.9505 * b
    return x, y, z


def xyz_to_linear_rgb(x: float, y: float, z: float) -> Vec3:
    """
    Convert CIE XYZ to linear RGB.
    """
    # fmt: off
    r =  3.2406 * x - 1.5372 * y - 0.4986 * z
    g = -0.9689 * x + 1.8758 * y + 0.0415 * z
    b =  0.0557 * x - 0.2040 * y + 1.0570 * z
    # fmt: on
    return r, g, b


def xyz_to_cieluv(x: float, y: float, z: float) -> Vec3:
    """
    Convert CIE XYZ to CIELUV. Input values are in [0, 1], output values in
    [0, 100] for L* and [-100, 100] for u* and v*.
    """
    assert (x, y, z) != (0.0, 0.0, 0.0), "Cannot convert black."

    if y < (6 / 29) ** 3:
        l_s = ((29 / 3) ** 3) * y
    else:
        l_s = 116 * (y ** (1 / 3)) - 16

    u_p = 4 * x / (x + 15 * y + 3 * z)
    v_p = 9 * y / (x + 15 * y + 3 * z)
    u_pn = 0.2009
    v_pn = 0.4610

    u_s = 13 * l_s * (u_p - u_pn)
    v_s = 13 * l_s * (v_p - v_pn)

    return l_s, u_s, v_s


def cieluv_to_xyz(l_s: float, u_s: float, v_s: float) -> Vec3:
    """
    Convert CIELUV to CIE XYZ. Inverse of `xyz_to_cieluv`.
    """
    u_pn = 0.2009
    v_pn = 0.4610

    u_p = u_s / (13 * l_s) + u_pn
    v_p = v_s / (13 * l_s) + v_pn

    if l_s <= 8:
        y = l_s * (3 / 29) ** 3
    else:
        y = ((l_s + 16) / 116) ** 3

    x = y * (9 * u_p) / (4 * v_p)
    z = y * (12 - 3 * u_p - 20 * v_p) / (4 * v_p)

    return x, y, z


def cieluv_to_cielch(l_s: float, u_s: float, v_s: float) -> Vec3:
    """
    Convert CIELUV to CIELCh. Returns (L*, C*_uv, h_uv).
    h is in the range [0, 2pi].
    """
    c_s = math.hypot(u_s, v_s)
    h = math.atan2(v_s, u_s)
    return l_s, c_s, h


def cielch_to_cieluv(l_s: float, c_s: float, h: float) -> Vec3:
    """
    Inverse of `cieluv_to_cielch`.
    """
    u_s = math.cos(h) * c_s
    v_s = math.sin(h) * c_s
    return l_s, u_s, v_s


class Color(NamedTuple):
    """
    A sRGB color with integer components in [0, 255].
    """

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
        assert len(hex) == 7
        rgb = bytes.fromhex(hex[1:])
        return Color(rgb[0], rgb[1], rgb[2])

    def to_html_hex(self) -> str:
        """
        Format as # and six hexadecimal digits, e.g. #ff0000.
        """
        return f"#{self.r:02x}{self.g:02x}{self.b:02x}"

    def to_rgb_floats(self) -> Vec3:
        """
        Return a tuple with float rgb values in the range [0, 1].
        """
        return self.r / 255.0, self.g / 255.0, self.b / 255.0

    @staticmethod
    def from_rgb_floats(r: float, g: float, b: float) -> Color:
        """
        Create a color from floats between 0 and 1.
        """
        return Color(
            min(255, max(0, int(r * 255.0))),
            min(255, max(0, int(g * 255.0))),
            min(255, max(0, int(b * 255.0))),
        )

    def to_cielch(self) -> Vec3:
        r, g, b = self.to_rgb_floats()
        return cieluv_to_cielch(
            *xyz_to_cieluv(
                *linear_rgb_to_xyz(
                    srgb_to_linear(r),
                    srgb_to_linear(g),
                    srgb_to_linear(b),
                )
            )
        )

    @staticmethod
    def from_cielch(c_s: float, h: float, s: float) -> Color:
        r, g, b = xyz_to_linear_rgb(*cieluv_to_xyz(*cielch_to_cieluv(c_s, h, s)))
        return Color.from_rgb_floats(
            linear_to_srgb(r),
            linear_to_srgb(g),
            linear_to_srgb(b),
        )

    def to_cieluv(self) -> Vec3:
        r, g, b = self.to_rgb_floats()
        return xyz_to_cieluv(
            *linear_rgb_to_xyz(
                srgb_to_linear(r),
                srgb_to_linear(g),
                srgb_to_linear(b),
            )
        )

    @staticmethod
    def from_cieluv(l_s: float, u_s: float, v_s: float) -> Color:
        r, g, b = xyz_to_linear_rgb(*cieluv_to_xyz(l_s, u_s, v_s))
        return Color.from_rgb_floats(
            linear_to_srgb(r),
            linear_to_srgb(g),
            linear_to_srgb(b),
        )

    def clamp_for_ui(self) -> Color:
        """
        Limit saturation and lightness to make the color suitable for usage in the UI.
        """
        l, c, h = self.to_cielch()
        l = max(45, min(65, l))
        c = max(50, min(65, c))
        return Color.from_cielch(l, c, h)
