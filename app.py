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


# Not sure why Mypy does not like this, maybe because I am mixing named
# tuples with Flask's tuple, and usually that works, but maybe deep inside
# Callable it creates problems?
@app.errorhandler(NotLoggedInError)  # type: ignore
def handle_not_logged_in(_: NotLoggedInError) -> Response:
    return Response.redirect_see_other("/login")
