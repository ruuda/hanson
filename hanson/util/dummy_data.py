#!/usr/bin/env python3

from hanson.database import Transaction, connect_default
from hanson.models.user import User
from hanson.models.session import Session
from typing import List


def add_users(tx: Transaction) -> List[User]:
    henk = User.create(tx, "henk")
    piet = User.create(tx, "piet")
    return [henk, piet]


def add_sessions(tx: Transaction, users: List[User]) -> List[Session]:
    return [Session.create(tx, user.id) for user in users]


def main() -> None:
    conn = connect_default()
    with conn.begin() as tx:
        users = add_users(tx)
        _sessions = add_sessions(tx, users)
        tx.commit()


if __name__ == "__main__":
    main()
