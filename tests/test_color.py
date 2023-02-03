from hanson.util.color import Color


def test_from_to_html_hex_roundtrips() -> None:
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
    c = Color.from_html_hex("#123456")
