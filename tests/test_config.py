from hanson.models.config import Config, PostgresConfig, ServerConfig


def test_config_parses_repo_default_config() -> None:
    config = Config.load_from_toml_file("config.toml")
    assert config == Config(
        postgres=PostgresConfig(
            database="hanson",
            user="hanson_app",
            password="hanson_app",
            host="./run/db_dev",
        ),
        server=ServerConfig(
            host="127.0.0.1",
            port=5471,
        ),
    )
