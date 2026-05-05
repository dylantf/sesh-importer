# OCaml is pain

## Prerequisites

The repo's top-level `flake.nix` provides the OCaml compiler, opam, and the
system libs (gmp, pkg-config, postgresql/libpq). Enter the dev shell with
`nix develop` (or `direnv allow` if you use direnv).

Everything else — dune, findlib, ocaml-lsp-server, ocamlformat, and the
project's library deps — is managed by opam in a local switch. Mixing
nixpkgs-managed OCaml tooling with opam-installed libraries doesn't work
(findlib destdir conflicts), and nixpkgs is behind on dune.

## Setup

```bash
# Create a local switch that uses the Nix-provided OCaml compiler.
opam switch create . --empty
eval $(opam env)

# Install dune + tooling, then project deps.
opam install dune ocaml-lsp-server ocamlformat --no-depexts
dune build importer.opam            # generate opam file from dune-project
opam install --deps-only . --no-depexts
```

`--no-depexts` skips opam's system-package check; the flake already provides
gmp, pkg-config, and postgresql.

Optional: drop a `.envrc` here so direnv loads the opam switch automatically:

```
use flake ..
eval "$(opam env)"
```

## Usage

```bash
dune exec importer
```
