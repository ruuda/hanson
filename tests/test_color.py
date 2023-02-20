from hanson.models.color import Color, Vec3
from hanson.models import color


def test_from_to_html_hex_roundtrip() -> None:
    def roundtrip(x: str) -> str:
        return Color.from_html_hex(x).to_html_hex()

    assert roundtrip("#ff0000") == "#ff0000"
    assert roundtrip("#00ff00") == "#00ff00"
    assert roundtrip("#0000ff") == "#0000ff"


def test_from_html_hex() -> None:
    c = Color.from_html_hex("#123456")
    assert c.r == 0x12
    assert c.g == 0x34
    assert c.b == 0x56


def test_to_html_hex() -> None:
    c = Color(0x12, 0x34, 0x56)
    assert c.to_html_hex() == "#123456"


def test_sgrb_linear_roundtrip() -> None:
    def roundtrip(v: float) -> float:
        return color.linear_to_srgb(color.srgb_to_linear(v))

    def roundtrip_diff(v: float) -> float:
        return abs(v - roundtrip(v))

    for i in range(100):
        assert roundtrip_diff(i / 100) < 1e-5


def test_linear_rgb_xyz_roundtrip() -> None:
    def roundtrip_diff(v: Vec3) -> float:
        w = color.xyz_to_linear_rgb(*color.linear_rgb_to_xyz(*v))
        return sum(abs(vi - wi) for vi, wi in zip(v, w))

    for ri in range(50):
        for gi in range(50):
            for bi in range(50):
                assert roundtrip_diff((ri / 50, gi / 50, bi / 50)) < 1e-4


def test_xyz_to_cieluv_roundtrip() -> None:
    def roundtrip_diff(v: Vec3) -> float:
        w = color.cieluv_to_xyz(*color.xyz_to_cieluv(*v))
        return sum(abs(vi - wi) for vi, wi in zip(v, w))

    for xi in range(1, 50):
        for yi in range(1, 50):
            for zi in range(1, 50):
                assert roundtrip_diff((xi / 50, yi / 50, zi / 50)) < 1e-5


def test_cieluv_to_cielch_roundtrip() -> None:
    def roundtrip_diff(v: Vec3) -> float:
        w = color.cielch_to_cieluv(*color.cieluv_to_cielch(*v))
        return sum(abs(vi - wi) for vi, wi in zip(v, w))

    for xi in range(1, 50):
        for yi in range(1, 50):
            for zi in range(1, 50):
                assert roundtrip_diff((xi / 50, yi / 50, zi / 50)) < 1e-5


def test_color_to_cielch_roundtrip() -> None:
    def roundtrip(c: Color) -> Color:
        return Color.from_cielch(*c.to_cielch())

    for r in range(1, 255):
        for g in range(1, 255, 10):
            for b in range(1, 255, 10):
                c = Color(r, g, b)
                d = roundtrip(c)
                # The conversion is not exact, mostly due to the sRGB
                # part missing some decimals on my part.
                assert sum(abs(ci - di) for ci, di in zip(c, d)) < 3
