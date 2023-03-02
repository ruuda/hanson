# Deploying

Hanson is a Python application built with [Flask][flask]. It uses Postgres 14 as
its database.

[flask]: https://flask.palletsprojects.com/en/2.2.x/

## Database

To set up the database, use the included migration tool in `tools/migrate.py`.
See the [development guide](development.md#database-setup) for more information.

## Packaging

The repository is a Nix flake. TODO: Possibly add a container image build output.
