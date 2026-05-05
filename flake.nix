{
  description = "sesh-importer polyglot dev environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    saga.url = "github:dylantf/saga";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      saga,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # F#
            dotnet-sdk_10
            fantomas

            # Gleam
            gleam
            erlang_27
            rebar3

            # Haskell
            haskell.compiler.ghc910
            cabal-install
            (haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
            postgresql.pg_config # for postgresql-simple
            zlib

            # OCaml
            ocaml-ng.ocamlPackages_5_4.ocaml
            pkg-config # opam build tooling
            opam
            gmp
            postgresql # libpq for caqti-driver-postgresql

            # Rust
            cargo
            rustc
            rustfmt
            clippy
            rust-analyzer

            # TypeScript
            bun
            nodejs_22
            typescript

            saga.packages.${system}.default
          ];

          shellHook = ''
            echo "sesh-importer dev shell"
          '';
        };
      }
    );
}
