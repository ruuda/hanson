from flask import Flask

from hanson.http import Response
from hanson.routes import index
from hanson.routes import market
from hanson.routes import session
from hanson.util.session import NotLoggedInError

app = Flask(import_name="hanson")
app.register_blueprint(index.app)
app.register_blueprint(market.app)
app.register_blueprint(session.app)


@app.errorhandler(NotLoggedInError)
def handle_not_logged_in(_: NotLoggedInError) -> Response:
    return Response.redirect_see_other("/login")
