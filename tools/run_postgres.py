#!/usr/bin/env python3

# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Run a development instance of Postgres.

Usage: ./run_postgres.py [--force-init] <data_dir>
"""

import os
import subprocess
import sys
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
    shutil.rmtree(f"{data_dir}", ignore_errors=True)

is_initialized = os.path.isdir(f"{data_dir}/pgdata")

if not is_initialized:
    os.makedirs(f"{data_dir}/pgdata", exist_ok=True)

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
        env={"TZ": "Etc/UTC", **os.environ},
        check=True,
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
        "..",
        # Set the listen address to empty string, we only want to listen on a
        # Unix socket, not on a port.
        "-c",
        "listen_addresses=",
        "-c",
        "timezone=UTC",
    ],
)
