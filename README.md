# Hanson

Hanson is a [prediction market][prediction-market] app.

 * Self-hosted, free and open-source software.
 * No real money, users trade virtual internet points.
 * Designed for running internal prediction markets in organizations.

**Vaporware warning**: Hanson is under development and not yet in a state where
it is useful. I will likely lose interest in it before it reaches that stage.

## Getting started

See [the development guide][docs-dev] for a detailed guide for how to run and
develop Hanson locally. The gist of it is:

Enter a development environment with [Nix][nix] [≥2.11][nix-2.11]:

    nix develop --command $SHELL

Run the initial database setup:

    tools/run_postgres.py --force-init run/db_dev

    # In a different terminal:
    export PGHOST="$PWD/run/db_dev"
    tools/migrate.py setup
    tools/migrate.py migrate latest

    # Now you can Ctrl+C the postgres instance in the other terminal.

Run the app in development mode:

    overmind start

By default, Flask will listen on `http://localhost:5000`. Then create yourself
a few users, and give them some points to spend:

    ./cli.py add-user etyrell   "Eldon Tyrell"
    ./cli.py add-user lkowalski "Leon Kowalski"
    ./cli.py add-user rbatty    "Roy Batty"
    ./cli.py add-user rdeckard  "Rick Deckard"
    ./cli.py airdrop 25.0

Hanson intends to outsource authentication to a third-party identity provider,
so there is no authentication step for development, you can sign in with only
the username.

[docs-dev]:          https://docs.ruuda.nl/hanson/development/
[nix-2.11]:          https://releases.nixos.org/?prefix=nix/nix-2.11.0/
[nix]:               https://nixos.org/
[prediction-market]: https://en.wikipedia.org/wiki/Prediction_market

## To do

 * Add a vacuum daemon because autovacuum doesn’t run on insert-only tables.
 * Add the ability to freeze a market, to prevent insider trading by the people
   who have control over the resolution.
 * Add a way for anybody to contribute an outcome (in the case of categorical
   outcomes), so markets can be used somewhat like open-answer polls.

## License

Hanson is licensed under the [Apache 2.0][apache2] license. Please do not open
an issue if you disagree with the choice of license.

[apache2]: https://www.apache.org/licenses/LICENSE-2.0
