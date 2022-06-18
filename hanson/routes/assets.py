# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from flask import Blueprint, render_template

from hanson.database import Transaction
from hanson.http import Response
from hanson.util.decorators import with_tx
from hanson.util.session import get_session_user
from hanson.models.asset_report import AssetReport

app = Blueprint(name="assets", import_name=__name__)


@app.get("/assets")
@with_tx
def route_get_assets(tx: Transaction) -> Response:
    session_user = get_session_user(tx)
    asset_report = AssetReport.get_for_user(tx, session_user.user.id)
    return Response.ok_html(
        render_template(
            "assets.html",
            session_user=session_user,
            asset_report=asset_report,
        )
    )
