#!/usr/bin/env python3

"""
Run a development instance of Postgres.

Usage: ./run_postgres.py [--force-init] <data_dir>
"""

import os
import subprocess
import sys
import textwrap
import shutil

force_init = False
data_dir = "run/db"

if len(sys.argv) < 2:
    print(__doc__.strip())
    sys.exit(1)

for arg in sys.argv[1:]:
    if arg == "--force-init":
        force_init = True
    else:
        data_dir = arg

if force_init:
    shutil.rmtree(f"{data_dir}/pgdata", ignore_errors=True)
    shutil.rmtree(f"{data_dir}/socket", ignore_errors=True)

is_initialized = os.path.isdir(f"{data_dir}/pgdata")

if not is_initialized:
    os.makedirs(f"{data_dir}/pgdata", exist_ok=True)
    os.makedirs(f"{data_dir}/socket", exist_ok=True)

    # Write a password file for 'initdb' to read from.
    with open(f"{data_dir}/password", "w", encoding="utf-8") as f:
        f.write("postgres\n")

    subprocess.run(
        [
            "initdb",
            "--locale=C",
            "--encoding=UTF-8",
            "--username=postgres",
            f"--pwfile={data_dir}/password",
            f"--pgdata={data_dir}/pgdata",
        ],
        check=True,
    )

    # Overwrite the default generated config files with these.
    with open(f"{data_dir}/pgdata/pg_hba.conf", "w", encoding="utf-8") as f:
        f.write(
            textwrap.dedent(
                """
                # This file specifies Postgres authentication per database and
                # role. For the development environment, we use password
                # authentication, because it is easiest to set up.

                #      database   user          auth-method
                local  all        postgres      md5
                local  hanson     hanson_app    md5
                local  hanson     hanson_setup  md5
                """
            )
        )

    with open(f"{data_dir}/pgdata/pg_ident.conf", "w", encoding="utf-8") as f:
        f.write(
            textwrap.dedent(
                """
                # This file maps Unix usernames to Postgres role names.
                # This is for peer authentication, which we don’t use.
                # Hence we leave this file empty.
                """
            )
        )

    with open(f"{data_dir}/pgdata/postgresql.conf", "w", encoding="utf-8") as f:
        f.write(
            textwrap.dedent(
                """
                # Do not listen on a TCP socket; we connect to Postgres over a
                # Unix socket.
                listen_addresses = ''
                timezone = 'UTC'
                """
            )
        )


os.chdir(f"{data_dir}/pgdata")
os.execvp(
    file="postgres",
    args=[
        "postgres",
        # PGDATA directory
        "-D",
        ".",
        # Directory to put the Unix socket in to listen on.
        "-k",
        "../socket",
    ],
)
