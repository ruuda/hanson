#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

import sys

from flask import Flask

from hanson.http import Response
from hanson.models.config import Config
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


@app.errorhandler(NotLoggedInError)
def handle_not_logged_in(_: NotLoggedInError) -> Response:
    return Response.redirect_see_other("/login")


def main() -> None:
    """
    Run Hanson in production mode through the Waitress WSGI server.

    This app.py is designed to run under Flask in development mode, you can run
    with "python -m flask run". In that case, this main function is not called.
    When executing app.py directly, Waitress is used.
    """
    import waitress  # type: ignore
    import textwrap

    if len(sys.argv) != 2:
        assert main.__doc__ is not None
        print(textwrap.dedent(main.__doc__).strip())
        print("\nUsage: app.py <config-toml>")
        sys.exit(1)

    config = Config.load_from_toml_file(sys.argv[1])
    app.config["hanson_config"] = config

    print("Configuration:")
    print(config.format_echo())

    print("Starting server ...")
    waitress.serve(app, host=config.server.host, port=config.server.port)


if __name__ == "__main__":
    main()

else:
    # When running through Flask, load the config file from a hard-coded location.
    app.config["hanson_config"] = Config.load_from_toml_file("config.toml")
