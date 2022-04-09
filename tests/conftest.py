# Mypy does not understand the `setup_out` kwargs to subprocess.run throughout
# this file, so silence those errors. Also, Mypy says this comment is unused,
# but it is not.

"""
This module defines Pytest fixtures. The main fixtures are `db_connection` and
`tx`. For the tests, we spawn an entirely new Postgres instance per test
session, so tests don't interfere with the development database.
"""

import os
import subprocess
import time
from subprocess import DEVNULL, Popen
from typing import Any, Dict, Iterable

import pytest

from hanson.database import ConnectionPool, Transaction
from hanson.models.market import Market
from hanson.models.user import User

pg_host = f"{os.getcwd()}/run/db_test/socket"
pg_env: Dict[str, str] = {
    "PATH": os.getenv("PATH") or "",
    "PGDATABASE": "hanson",
    "PGUSER": "hanson_setup",
    "PGPASS": "hanson_setup",
    "PGHOST": pg_host,
}

# Default to silencing all database setup output, because it makes the tests
# super noisy, pytest captures all of it. But should we need to debug the setup
# code, we can change it here. We have to give this type signature here because
# otherwise Mypy complains loudly about the `**setup_out` kwargs.
setup_out: Dict[str, Any] = {
    "stdout": DEVNULL,
    "stderr": DEVNULL,
}


def run_postgres() -> Popen[bytes]:
    """
    Start a Postgres process and wait for it to be ready before returning it.
    """
    cmd_postgres = ["tools/run_postgres.py", "--force-init", "run/db_test"]
    postgres: Popen[bytes] = Popen(cmd_postgres, **setup_out)

    # Wait for Postgres to be available.
    start_second = time.monotonic()
    wait_seconds = 10.0
    while True:
        cmd_check = ["pg_isready"]
        result = subprocess.run(cmd_check, env=pg_env, **setup_out)
        if result.returncode == 0:
            break

        if time.monotonic() - start_second < wait_seconds:
            sleep_seconds = 0.1
            time.sleep(sleep_seconds)
            continue

        postgres.kill()
        raise Exception(f"Postgres is not online after {wait_seconds}s.")

    return postgres


@pytest.fixture(scope="session")
def db_connection() -> Iterable[ConnectionPool]:
    with run_postgres() as postgres:
        cmd_setup = ["tools/setup_database.py"]
        subprocess.run(cmd_setup, env=pg_env, check=True, **setup_out)

        cmd_migrate = ["tools/migrate.py", "migrate", "latest"]
        subprocess.run(cmd_migrate, env=pg_env, check=True, **setup_out)

        yield ConnectionPool.new(
            database="hanson",
            user="hanson_app",
            password="hanson_app",
            host=pg_host,
        )

        # Just .terminate() doesn't appear to stop Postgres, we need to kill it.
        postgres.kill()


@pytest.fixture(scope="function")
def tx(db_connection: ConnectionPool) -> Iterable[Transaction]:
    with db_connection.begin() as conn:
        yield conn


@pytest.fixture(scope="function")
def user(tx: Transaction) -> Iterable[User]:
    yield User.create(
        tx,
        username="tester",
        full_name="Test User One",
    )


@pytest.fixture(scope="function")
def market(tx: Transaction, user: User) -> Iterable[Market]:
    yield Market.create(
        tx,
        author_user_id=user.id,
        title="Test Market",
        description="The default market for use in tests.",
    )
