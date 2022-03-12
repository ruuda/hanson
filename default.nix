# This file defines a Nix environment in which all required build tools and
# dependencies are available. You do not *need* to use it, you can install
# dependencies in any way you see fit. The advantage of the Nix environment is
# that the Nixpkgs revision is pinned, and thereby the versions of all tools. If
# you can build a commit today, you should be able to build it three years from
# now. The same may not be true if you use the distro-provided versions. You can
# start a shell with dependencies available by running `nix run` in the root of
# the repository.

let
  pkgs = (import ./nixpkgs-pinned.nix) {};
  runtimeDependencies = ps: [
    ps.flask
    ps.jinja2
    ps.psycopg2
  ];
  developmentDependencies = ps: [
    # Mypy goes with the pythonPackages, we don't use the top-level pkgs.mypy,
    # because that one is not able to find type hints of dependencies, and if
    # we include it like this, then Mypy *can* typecheck Flask functions.
    ps.mypy
  ];
  python = pkgs.python3.withPackages (ps:
    (runtimeDependencies ps) ++ (developmentDependencies ps)
  );
in
  pkgs.buildEnv {
    name = "hanson-devenv";
    paths = [
      pkgs.black
      pkgs.overmind
      pkgs.postgresql_14
      python
    ];
  }
