#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from flask import Flask

from hanson.http import Response
from hanson.routes import assets as route_assets
from hanson.routes import index as route_index
from hanson.routes import market as route_market
from hanson.routes import session as route_session
from hanson.routes import user as route_user
from hanson.util.session import NotLoggedInError

app = Flask(import_name="hanson")
app.register_blueprint(route_assets.app)
app.register_blueprint(route_index.app)
app.register_blueprint(route_market.app)
app.register_blueprint(route_session.app)
app.register_blueprint(route_user.app)


# Not sure why Mypy does not like this, maybe because I am mixing named
# tuples with Flask's tuple, and usually that works, but maybe deep inside
# Callable it creates problems?
@app.errorhandler(NotLoggedInError)
def handle_not_logged_in(_: NotLoggedInError) -> Response:
    return Response.redirect_see_other("/login")


def main() -> None:
    """
    This app.py is designed to run under Flask in development mode, you can run
    with "python -m flask run". In that case, this main function is not called.

    For a production deployment, we use Waitress as the WSGI server. Then we can
    start from app.py as the entry point.
    """
    import os
    import waitress  # type: ignore

    # TODO: Maybe put this in a config file after all?
    # The current values are misleading because we don't use those!
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", "5000"))
    pg_host = os.getenv("PGHOST", f"{os.getcwd()}/run/db_dev")
    pg_database = os.getenv("PGDATABASE", "hanson")
    pg_user = os.getenv("PGUSER", "hanson_app")
    pg_password = os.getenv("PGPASSWORD", "hanson_app")

    print("Configuration:")
    print(f"  BIND={host}")
    print(f"  PORT={port}")
    print(f"  PGDATABASE={pg_database}")
    print(f"  PGUSER={pg_user}")
    print(f"  PGPASSWORD is not printed here")
    print(f"  PGHOST={pg_host}")
    print("Starting server ...")
    waitress.serve(app, host=host, port=port)


if __name__ == "__main__":
    main()
