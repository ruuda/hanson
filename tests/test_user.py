from hanson.models.user import User


def test_user_is_valid_username() -> None:
    assert not User.is_valid_username("")
    assert not User.is_valid_username("Foo")
    assert not User.is_valid_username("bar123")
    assert not User.is_valid_username("with space")
    assert not User.is_valid_username("with-dash")
    assert not User.is_valid_username("frédérique")
    assert not User.is_valid_username("σωκράτης")

    assert User.is_valid_username("hankthestone")
    assert User.is_valid_username("frederique")
    assert User.is_valid_username("socrates")


def test_user_validate_full_name_get_violation() -> None:
    assert User.validate_full_name_get_violation("Hank the Stone") is None
    assert User.validate_full_name_get_violation("Frédérique") is None
    assert User.validate_full_name_get_violation("Jean-Luc Picard") is None
    assert User.validate_full_name_get_violation("Σωκράτης") is None
    assert User.validate_full_name_get_violation("Алекса́ндр") is None
    assert User.validate_full_name_get_violation("神武天皇") is None
    assert User.validate_full_name_get_violation("Greg O’Brien") is None

    assert User.validate_full_name_get_violation("House, M.D.") == ","
    assert User.validate_full_name_get_violation("Invalid!") == "!"
    assert User.validate_full_name_get_violation("\n") == "\n"
    assert User.validate_full_name_get_violation("\U0001f574") == "\U0001f574"
    assert User.validate_full_name_get_violation("Official Admin ♦") == "♦"

    # This one is unfortunate, but you have to use the ’ instead.
    assert User.validate_full_name_get_violation("Greg O'Brien") == "'"
