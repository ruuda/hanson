# Hanson -- Self-hosted prediction market app
# Copyright 2023 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

import os
import tomllib

from typing import NamedTuple


class PostgresConfig(NamedTuple):
    database: str
    user: str
    password: str
    host: str


class ServerConfig(NamedTuple):
    host: str
    port: int


class Config(NamedTuple):
    postgres: PostgresConfig
    server: ServerConfig

    @staticmethod
    def load_from_toml_file(fname: str) -> Config:
        with open(fname, "rb") as f:
            raw = tomllib.load(f)
            assert "postgres" in raw
            assert "server" in raw

            if "password" not in raw["postgres"]:
                password = os.getenv("PGPASSWORD")
                assert password is not None, (
                    "Postgres password must either be set in the config "
                    "or be provided through PGPASSWORD."
                )
                raw["postgres"]["password"] = password

            return Config(
                postgres=PostgresConfig(**raw["postgres"]),
                server=ServerConfig(**raw["server"]),
            )
