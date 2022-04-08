from __future__ import annotations

import contextlib
import os
from typing import Any, Dict, Iterator, Iterable, Optional, NamedTuple, Tuple, Union

import psycopg2.extensions  # type: ignore
import psycopg2.extras  # type: ignore
import psycopg2.pool  # type: ignore


class Transaction:
    def __init__(self, conn: psycopg2.extensions.connection) -> None:
        self.conn = conn

    def commit(self) -> None:
        self.conn.commit()
        # Ensure we cannot reuse the connection. (This is where moves like in
        # Rust would have been nice ...)
        self.conn = None

    def rollback(self) -> None:
        self.conn.rollback()
        self.conn = None

    def cursor(self) -> psycopg2.extensions.cursor:
        return self.conn.cursor()

    def execute(
        self,
        query: str,
        params: Union[None, Tuple[Any, ...], Dict[str, Any]] = None,
    ) -> None:
        """Execute a query that does not return results."""
        with self.cursor() as cur:
            cur.execute(query, params)

    def execute_fetch_optional(
        self,
        query: str,
        params: Union[None, Tuple[Any, ...], Dict[str, Any]] = None,
    ) -> Optional[Tuple[Any, ...]]:
        """Execute a query and return the first result."""
        with self.cursor() as cur:
            cur.execute(query, params)
            result: Optional[Tuple[Any, ...]] = cur.fetchone()
            return result

    def execute_fetch_one(
        self,
        query: str,
        params: Union[None, Tuple[Any, ...], Dict[str, Any]] = None,
    ) -> Tuple[Any, ...]:
        """
        Execute a query and return the resulting row, fail if there is no result.
        """
        result = self.execute_fetch_optional(query, params)
        assert result is not None, "Expected exactly one result."
        return result

    def execute_fetch_scalar(
        self,
        query: str,
        params: Union[None, Tuple[Any, ...], Dict[str, Any]] = None,
    ) -> Any:
        """Execute a query and return the result, fail if there is no result."""
        result = self.execute_fetch_one(query, params)
        assert len(result) == 1, "Expected a result with a single column."
        return result[0]

    def execute_fetch_all(
        self,
        query: str,
        params: Union[None, Tuple[Any, ...], Dict[str, Any]] = None,
    ) -> Iterable[Tuple[Any, ...]]:
        """Execute a query and iterate all results."""
        with self.cursor() as cur:
            cur.execute(query, params)
            while True:
                result: Optional[Tuple[Any, ...]] = cur.fetchone()

                if result is None:
                    return

                yield result


class ConnectionPool(NamedTuple):
    pool: psycopg2.pool.ThreadedConnectionPool

    @staticmethod
    def new(
        database: str,
        user: str,
        password: str,
        host: str,
        port: Optional[int] = None,
    ) -> ConnectionPool:
        # Enable UUID support for Psycopg2.
        psycopg2.extras.register_uuid()

        pool = psycopg2.pool.ThreadedConnectionPool(
            minconn=1,
            maxconn=10,
            database=database,
            user=user,
            password=password,
            host=host,
            port=port,
        )
        return ConnectionPool(pool)

    @contextlib.contextmanager
    def begin(self) -> Iterator[Transaction]:
        conn: Optional[psycopg2.extensions.connection] = None
        try:
            # Use psycopg2 in "no-autocommit" mode, where it implicitly starts a
            # transaction at the first statement, and we need to explicitly
            # commit() or rollback() afterwards.
            conn = self.pool.getconn()
            conn.isolation_level = "SERIALIZABLE"
            conn.autocommit = False
            yield Transaction(conn)

        except:
            if conn is not None:
                self.pool.putconn(conn, close=True)
            raise

        else:
            assert conn is not None
            self.pool.putconn(conn, close=False)


def connect_default() -> ConnectionPool:
    return ConnectionPool.new(
        database="hanson",
        user="hanson_app",
        password="hanson_app",
        host=f"{os.getcwd()}/run/db_dev/socket",
    )


def get_context_connection() -> ConnectionPool:
    """
    Return the global database connection associated with the Flask app.
    """
    from flask import _app_ctx_stack

    ctx = _app_ctx_stack.top
    assert ctx is not None, "Must be called from within a Flask context."

    if not hasattr(ctx, "db_connection"):
        ctx.db_connection = connect_default()

    conn: ConnectionPool = ctx.db_connection
    return conn
