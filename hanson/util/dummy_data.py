#!/usr/bin/env python3

from hanson.database import Transaction, connect_default
from hanson.models.user import User


def add_users(tx: Transaction) -> None:
    henk = User.create(tx, "henk")
    print(henk)
    piet = User.create(tx, "piet")
    print(piet)


def main() -> None:
    conn = connect_default()
    with conn.begin() as tx:
        add_users(tx)
        tx.commit()


if __name__ == "__main__":
    main()
