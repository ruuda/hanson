# Hanson -- Self-hosted prediction market app
# Copyright 2022 Ruud van Asseldonk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

from __future__ import annotations

from typing import Any, Callable, TypeVar, cast

from hanson import database as db

# The type annotations in this file are a bit of a lie;
# they don't return the same function, they return a function with
# fewer arguments. But Mypy can't express that.
F = TypeVar("F", bound=Callable[..., Any])


def with_tx(f: F) -> F:
    """
    Begins a new transaction every time the function is called,
    and passes it to the "tx" argument.
    """

    def wrapper(*args: Any, **kwargs: Any) -> Any:
        with db.get_context_connection().begin() as tx:
            result = f(*args, **kwargs, tx=tx)

            # At the end of a request handler, roll back the transaction
            # if it was not explicitly committed.
            if tx.conn is not None:
                tx.rollback()

            return result

    # Give the wrapper the same name as the original function,
    # otherwise the Flask decorators break.
    wrapper.__name__ = f.__name__
    return cast(F, wrapper)
