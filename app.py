from flask import Flask

from hanson.http import Response
from hanson.routes import index as route_index
from hanson.routes import market as route_market
from hanson.routes import session as route_session
from hanson.routes import user as route_user
from hanson.util.session import NotLoggedInError

app = Flask(import_name="hanson")
app.register_blueprint(route_index.app)
app.register_blueprint(route_market.app)
app.register_blueprint(route_session.app)
app.register_blueprint(route_user.app)


# Not sure why Mypy does not like this, maybe because I am mixing named
# tuples with Flask's tuple, and usually that works, but maybe deep inside
# Callable it creates problems?
@app.errorhandler(NotLoggedInError)  # type: ignore
def handle_not_logged_in(_: NotLoggedInError) -> Response:
    return Response.redirect_see_other("/login")
