# Configuration

Hanson can be configured using a <abbr>TOML</abbr> configuration file. The
repository includes a sample `config.toml` which is the default configuration
for local development. For convenience, the example config is replicated here:

```toml
[postgres]
database = "hanson"
user = "hanson_app"
password = "hanson_app"
host = "./run/db_dev"

[server]
host = "127.0.0.1"
port = 5471
```

## Postgres

The `postgres` section configures the database connection. Hanson is developed
against Postgres 14.

### database

Name of the database to connect to, should be `hanson` if you set up the
database with the included migration tool.

### user

Database user to connect as, should be `hanson_app` if you set up the database
users with the included migration tool.

### password

Password for the database user. To avoid having to write the password in plain
text to disk, you may omit this key. In that case, Hanson will read the password
from the `PGPASSWORD` environment variable instead.

### host

Postgres server to connect to. This can be a hostname or <abbr>IP</abbr> address,
or it can be a path to a Unix domain socket. If the value starts with `.`, then
Hanson expands it to an absolute path.


## Server

The `server` section configures [Waitress][waitress], which is used as the
[<abbr>WSGI</abbr>][pep3333] webserver that serves the application. This section
is only used when running Hanson directly through `app.py`, it is not used in
local development when running through Flask.

### host

Hostname or <abbr>IP</abbr> address to bind to. Use `0.0.0.0` to listen on all
interfaces.

### port

Port to listen on.

[pep3333]:  https://peps.python.org/pep-3333/
[waitress]: https://docs.pylonsproject.org/projects/waitress/en/stable/index.html
