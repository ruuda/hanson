# Development guide

Hanson is written in Python and uses Postgres as its database.

## Development environment

To enter a development environment with the right version of Postgres, Python,
Python packages, and axiliary tools in scope, you can use [Nix][nix] version
[2.11][nix-2.11]. If you haven’t already, enable [flake support][flake] by
adding the following line to your `~/.config/nix/nix.conf`:

    experimental-features = nix-command flakes

Then enter a development shell with

    nix develop --command $SHELL

[nix]:      https://nixos.org/download.html
[nix-2.11]: https://releases.nixos.org/?prefix=nix/nix-2.11.0/
[flake]:    https://nixos.wiki/wiki/Flakes

## Database setup

The development environment includes a pinned version of Postgres. The script
`tools/run_postgres.py` starts a sufficiently configured Postgres process that
listens on a domain socket. Start one that puts its data in `run/db_dev`:

    tools/run_postgres.py --force-init run/db_dev

Then in a different shell, perform the initial setup that creates the database
and database users, and then run all migrations to populate the schema:

    export PGHOST="$PWD/run/db_dev"
    tools/migrate.py setup
    tools/migrate.py migrate latest

After this you can Ctrl + C the Postgres process in the other shell. You will
not need to start it manually any more, see also [_Running the
application_](#running-the-application) below.

The development shell by defaults sets the `PGDATABASE`, `PGUSER`, and
`PGPASSWORD` environment variables. If you export `PGHOST=$PWD/run/db_dev`,
then `psql` can be used directly to inspect the database.

## Migrations

The repository includes a simple migration tool, `tools/migrate.py`. It loads
migrations from the `migrations` directory.

Migrations must be `.sql` files whose filename starts with a 4-digit sequence
number. Each constists of an upgrade and downgrade section as indicated with a
comment:

```sql
-- migrate:up
CREATE TABLE "...";

-- migrate:down
DROP TABLE "...";
```

The migration tool stores the current schema version in the `_schema_migrations`
table. Run `tools/migrate.py --help` for more information.

## Running the application

The repository contains a [Procfile][procfile] that defines all processes needed
to run Hanson (Postgres and Flask). The development environment includes
[Overmind][overmind] which can start all processes in the Procfile, and which
interleaves their output on stdout. To bring everything up:

    overmind start

By default Flask listens on <http://localhost:5000>. However, the database does
not contain any users or interesting data. To add a few users and give them some
points to spend, use the admin <abbr>CLI</abbr> tool:

    ./cli.py add-user etyrell   "Eldon Tyrell"
    ./cli.py add-user lkowalski "Leon Kowalski"
    ./cli.py add-user rbatty    "Roy Batty"
    ./cli.py add-user rdeckard  "Rick Deckard"
    ./cli.py airdrop 25.0

After this you can log in by username. In development mode Hanson does not
authenticate users, the plan is to outsource this to a third-party identity
provider.

[procfile]: https://ddollar.github.io/foreman/#PROCFILE
[overmind]: https://github.com/DarthSim/overmind

## Running tests

Run the tests:

    python -m pytest tests

The tests will use an independent database at `run/db_test`, so the tests do not
interfere with the development database. The tests start this Postgres instance
in the test fixture, so this does not depend on any daemon to be running.

## Typechecking

Typecheck with [Mypy][mypy]:

    mypy --strict hanson tools tests

[mypy]: https://mypy-lang.org/

## Formatting

All code is formatted with [Black][black]:

    black hanson tests tools

[black]: https://github.com/psf/black
