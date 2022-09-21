{
  description = "Hanson";

  inputs = {
    nixpkgs.url = "nixpkgs/fcd48a5a0693f016a5c370460d0c2a8243b882dc";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: 
  utils.lib.eachDefaultSystem (system:
    let
      name = "noblit";
      version = "0.0.0";

      pkgs = import nixpkgs { inherit system; };

      runtimeDependencies = ps: [
        ps.click
        ps.flask
        ps.jinja2
        ps.psycopg2
      ];
      developmentDependencies = ps: [
        # Mypy goes with the pythonPackages, we don't use the top-level pkgs.mypy,
        # because that one is not able to find type hints of dependencies, and if
        # we include it like this, then Mypy *can* typecheck Flask functions.
        ps.mypy
        ps.pytest
      ];
      python = pkgs.python3.withPackages (ps:
        (runtimeDependencies ps) ++ (developmentDependencies ps)
      );
    in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "hanson";
            nativeBuildInputs = [
              pkgs.black
              pkgs.overmind
              pkgs.postgresql_14
              pkgs.mkdocs
              python
            ];
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          };
        };

        packages = {};
      }
  );
}
