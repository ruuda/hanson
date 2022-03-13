#!/usr/bin/env python3

"""
Create the Hanson database and users. Expects PGHOST to be set.
"""

import os
import subprocess

os.putenv("PGUSER", "postgres")
os.putenv("PGPASSWORD", "postgres")

os.putenv("PGDATABASE", "postgres")
subprocess.run(
    args=["psql"],
    check=True,
    input=b"""
    -- Create the role used by the application.
    CREATE ROLE hanson_app LOGIN PASSWORD 'hanson_app';

    -- Create the role used for migrations and administration.
    CREATE ROLE hanson_setup LOGIN PASSWORD 'hanson_setup';

    CREATE DATABASE hanson OWNER hanson_setup;
    """,
)

os.putenv("PGDATABASE", "hanson")
subprocess.run(
    args=["psql"],
    check=True,
    input=b"""
    -- By default, PostgreSQL creates a schema "public" that can be modified
    -- by any user ("PUBLIC"). We want more selective permissions.
    REVOKE ALL PRIVILEGES ON SCHEMA public FROM PUBLIC;
    GRANT USAGE ON SCHEMA public TO hanson_app;
    GRANT USAGE, CREATE ON SCHEMA public TO hanson_setup;

    -- The user "hanson_setup" will create new tables. When it creates them,
    -- "hanson_app" should be given access. So we need to modify the default
    -- privileges of "hanson_setup" to grant to "hanson_app".

    ALTER DEFAULT PRIVILEGES FOR ROLE hanson_setup
    GRANT SELECT, INSERT, UPDATE ON TABLES
    TO hanson_app;

    ALTER DEFAULT PRIVILEGES FOR ROLE hanson_setup
    GRANT EXECUTE ON FUNCTIONS
    TO hanson_app;
    """,
)
