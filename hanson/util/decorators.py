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

    def wrapper(*args, **kwargs):
        with db.get_context_connection().begin() as tx:
            return f(*args, **kwargs, tx=tx)

    # Give the wrapper the same name as the original function,
    # otherwise the Flask decorators break.
    wrapper.__name__ = f.__name__
    return cast(F, wrapper)
