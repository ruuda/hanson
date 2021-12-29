from flask import Flask, render_template

app = Flask("hanson")

@app.route("/")
@app.route("/~<name>")
def hello_world(name='visitor'):
    return render_template('index.html', name=name)
    return "<p>Hello, World!</p>"

