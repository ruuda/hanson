# web: FLASK_ENV=development python -m flask run --port $PORT
web: git ls-files | entr -r make run
db:  tools/run_postgres.py run/db_dev
