from flask import Flask

from hanson.routes import session
from hanson.routes import index

app = Flask(import_name="hanson")
app.register_blueprint(session.app)
app.register_blueprint(index.app)
