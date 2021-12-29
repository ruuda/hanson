from flask import Flask, render_template

app = Flask("hanson")


@app.route("/")
@app.route("/~<name>")
def hello_world(name: str = "visitor") -> str:
    return render_template("index.html", name=name)
