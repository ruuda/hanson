#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Run migrations to keep the database schema up to date.

Expects PGHOST to be set.
"""

from __future__ import annotations

import click
import os
import psycopg2  # type: ignore
import sys

from typing import Iterable, List, Optional, NamedTuple, Tuple
from collections import defaultdict


class Migration(NamedTuple):
    sql_up: str
    sql_down: str

    @staticmethod
    def from_file(fname: str) -> Migration:
        with open(fname, "r", encoding="utf-8") as f:
            mode = "prelude"
            sql_up: List[str] = []
            sql_down: List[str] = []
            for lineno, line in enumerate(f):
                loc = f"{fname}:{lineno+1}"

                if mode == "prelude":
                    if line.startswith("-- migrate:up"):
                        mode = "up"
                        continue
                    if line.startswith("-- migrate:down"):
                        print(f"Error in {loc}: Upgrade must come before downgrade.")
                        sys.exit(1)
                    elif line.startswith("--") or line.strip() == "":
                        continue
                    else:
                        print(f"Error in {loc}: Expected '-- migrate:up' comment.")
                        sys.exit(1)

                elif mode == "up":
                    if line.startswith("-- migrate:down"):
                        mode = "down"
                        continue
                    else:
                        sql_up.append(line)

                elif mode == "down":
                    if line.startswith("-- migrate:up"):
                        print(f"Error in {loc}: Upgrade must come before downgrade.")
                        sys.exit(1)
                    else:
                        sql_down.append(line)

        if len(sql_up) == 0:
            print(f"Error in {fname}: Expected non-empty '-- migrate:up' section.")
            sys.exit(1)

        if len(sql_down) == 0:
            print(f"Error in {fname}: Expected non-empty '-- migrate:down' section.")
            sys.exit(1)

        return Migration(
            sql_up="".join(sql_up),
            sql_down="".join(sql_down),
        )


class MigrationDef(NamedTuple):
    seq_no: int
    name: str
    fname: str

    def load(self) -> Migration:
        return Migration.from_file(f"migrations/{self.fname}")


def list_migrations() -> Iterable[MigrationDef]:
    for fname in os.listdir("migrations"):
        if not fname.endswith(".sql"):
            continue

        seq_no_str, name = fname.split("-", maxsplit=1)
        yield MigrationDef(int(seq_no_str), name.removesuffix(".sql"), fname)


def validate_migrations(defs: List[MigrationDef]) -> None:
    """
    Validate that:
    * The migration numbers form a contiguous range (no number is missing).
    * Every migration has a unique sequence number (no duplicate numbers).
    """
    by_number = defaultdict(lambda: [])

    for mig_def in defs:
        by_number[mig_def.seq_no].append(mig_def)

    for i in range(1, 1 + len(defs)):
        n = len(by_number[i])
        if n == 0:
            print(f"Error: Migration {i} is missing.")
            sys.exit(1)
        elif n == 1:
            continue
        else:
            print(f"Error: Migration {i} is not unique:")
            for mig_def in by_number[i]:
                print(f"  {mig_def.fname}")
            sys.exit(1)


def transaction() -> psycopg2.extensions.connection:
    return psycopg2.connect("dbname=hanson user=hanson_setup password=hanson_setup")


class Migrator(NamedTuple):
    revisions: List[MigrationDef]

    @staticmethod
    def new() -> Migrator:
        revisions = sorted(list_migrations())
        validate_migrations(revisions)
        return Migrator(revisions)

    def get_current_revision(self) -> int:
        try:
            with transaction() as tx:
                with tx.cursor() as cur:
                    cur.execute(
                        """
                        SELECT revision
                        FROM _schema_migrations
                        ORDER BY id DESC
                        LIMIT 1;
                        """
                    )
                    result: Optional[Tuple[int]] = cur.fetchone()
                    if result is None:
                        return 0
                    else:
                        return result[0]

        except psycopg2.errors.UndefinedTable:
            with transaction() as tx:
                with tx.cursor() as cur:
                    cur.execute(
                        """
                        CREATE TABLE _schema_migrations
                          ( id       BIGINT NOT NULL GENERATED BY DEFAULT AS IDENTITY
                          , revision BIGINT NOT NULL
                          , created  TIMESTAMPTZ NOT NULL DEFAULT now()
                          );
                        """
                    )
                tx.commit()
                return 0

    def parse_revision_spec(self, current: int, spec: str) -> int:
        if spec == "latest":
            return self.revisions[-1].seq_no

        elif spec.isdigit():
            return int(spec)

        elif spec.startswith("+") and spec[1:].isdigit():
            rel = int(spec[1:])
            return current + rel

        elif spec.startswith("-") and spec[1:].isdigit():
            rel = int(spec[1:])
            return current - rel

        else:
            print(f"Invalid revision specification: {spec}")
            sys.exit(1)

    def execute_migrations(
        self,
        rev_after: int,
        migrations: Iterable[Tuple[MigrationDef, str]],
    ) -> None:
        with transaction() as tx:
            with tx.cursor() as cur:
                for mig_def, sql in migrations:
                    print(f"{mig_def.seq_no:04} {mig_def.name}")
                    cur.execute(sql)

                cur.execute(
                    """
                    INSERT INTO _schema_migrations (revision) VALUES (%s);
                    """,
                    (rev_after,),
                )

            tx.commit()


@click.group()
def main() -> None:
    pass


@main.command()
def setup() -> None:
    """
    Create the database and roles. Must be run before running any migrations.
    This is idempotent and therefore safe to run multiple times.
    """
    import os
    import subprocess

    os.putenv("PGUSER", "postgres")
    os.putenv("PGPASSWORD", "postgres")
    os.putenv("PGDATABASE", "postgres")
    subprocess.run(args=["psql", "--file", "migrations/0000-initdb.psql"], check=True)


@main.command()
@click.argument("revision_spec", default="latest", required=False)
def migrate(revision_spec: str) -> None:
    """
    Migrate to a particular revision. Supported revision specifiers are:

    * An exact number, e.g. "12". Will upgrade or downgrade depending on the
      current revision.

    * A relative offset, e.g. "+1" or "-1". Will upgrade or downgrade this
      number of revisions relative to the current revision.

    * The "latest", which upgrades to the revision with the highest revision
      number. This is the default.
    """
    app = Migrator.new()

    current = app.get_current_revision()
    new_rev = app.parse_revision_spec(current, revision_spec)

    if new_rev < 0:
        print("Error: Revision numbers must not be negative.")
        sys.exit(1)

    elif new_rev > len(app.revisions):
        print(f"Error: Revision {new_rev} does not exist, max is {len(app.revisions)}.")
        sys.exit(1)

    if new_rev > current:
        print(f"Upgrading: {current:04} -> {new_rev:04}")
        # Note, the index is one lower than the revision at that index, so we
        # slice all new migrations that we still need to run.
        defs = app.revisions[current:new_rev]
        migs = [mig_def.load() for mig_def in defs]
        app.execute_migrations(new_rev, ((d, m.sql_up) for (d, m) in zip(defs, migs)))

    elif new_rev < current:
        print(f"Downgrading: {current:04} -> {new_rev:04}")
        defs = [d for d in reversed(app.revisions[new_rev:current])]
        migs = [mig_def.load() for mig_def in defs]
        app.execute_migrations(new_rev, ((d, m.sql_down) for (d, m) in zip(defs, migs)))


@main.command()
def list() -> None:
    """
    Print all known migrations.
    """
    migrations = sorted(list_migrations())
    validate_migrations(migrations)
    for i, name, fname in reversed(migrations):
        print(f"{i:04} {name}")


@main.command()
def status() -> None:
    """
    Show current revision and pending migrations.
    """
    app = Migrator.new()
    current = app.get_current_revision()
    latest = len(app.revisions)
    if current > 0:
        print(f"At:     {current:04} {app.revisions[current - 1].name}")
    else:
        print(f"At:     0000 (uninitialized)")
    print(f"Latest: {latest:04} {app.revisions[-1].name}")
    if latest > current:
        # Note, the index is one lower than the revision at that index,
        # so we slice all new migrations that we still need to run.
        defs = app.revisions[current:latest]
        print("Pending migrations:")
        for i, name, _fname in defs:
            print(f"  {i:04} {name}")


if __name__ == "__main__":
    main()
