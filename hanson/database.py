from __future__ import annotations

import psycopg2.pool  # type: ignore
import psycopg2.extensions  # type: ignore
import contextlib

from typing import Iterator, NamedTuple


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


class ConnectionPool(NamedTuple):
    pool: psycopg2.pool.ThreadedConnectionPool

    @staticmethod
    def new(connection_str: str) -> ConnectionPool:
        pool = psycopg2.pool.ThreadedConnectionPool(
            minconn=1,
            maxconn=10,
            dsn=connection_str,
        )
        return ConnectionPool(pool)

    @contextlib.contextmanager
    def begin(self) -> Iterator[Transaction]:
        try:
            # Use psycopg2 in "autocommit" mode, where it implicitly starts a
            # transaction at the first statement, and we need to explicitly
            # commit() or rollback() afterwards.
            conn = self.pool.getconn()
            conn.isolation_level = "SERIALIZABLE"
            conn.autocommit = False
            yield Transaction(conn)

        except:
            self.pool.putconn(conn, close=True)
            raise

        else:
            self.pool.putconn(conn, close=False)


def connect_default() -> ConnectionPool:
    return ConnectionPool.new("dbname=hanson user=hanson_app password=hanson_app")
