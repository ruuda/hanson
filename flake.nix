{
  description = "Hanson";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: 
  utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      runtimeDependencies = ps: [
        ps.click
        ps.flask
        ps.jinja2
        ps.psycopg2
        ps.waitress
      ];
      developmentDependencies = ps: [
        # Mypy goes with the pythonPackages, we don't use the top-level pkgs.mypy,
        # because that one is not able to find type hints of dependencies, and if
        # we include it like this, then Mypy *can* typecheck Flask functions.
        ps.mypy
        ps.pytest

        # These do not strictly need to be here, but if we import them into our
        # development shell from the top level, we get multiple Python
        # interpreters in there, it's nicer to only have a single one.
        ps.black
        ps.mkdocs
        ps.pygments
      ];

      python = pkgs.python311.override {
        packageOverrides = self: super: {
          hanson = self.buildPythonPackage rec {
            pname = "hanson";
            version = "0.0.0";
            src = ./.;
            propagatedBuildInputs = runtimeDependencies self;
          };
        };
      };

      # For development, we want a Python that has all our dependent packages,
      # but not Hanson itself as a package.
      pythonDev = python.withPackages (ps:
        (runtimeDependencies ps) ++ (developmentDependencies ps)
      );

      hanson = python.pkgs.toPythonApplication python.pkgs.hanson;
    in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "hanson";

            nativeBuildInputs = [
              pkgs.overmind
              pkgs.postgresql_14
              pythonDev
            ];

            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

            # Some default env vars to make psql a bit easier to use.
            PGDATABASE = "hanson";
            PGUSER = "hanson_setup";
            PGPASSWORD = "hanson_setup";
          };
        };

        packages = {
          default = hanson;
        };
      }
  );
}
