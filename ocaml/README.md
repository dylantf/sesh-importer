# OCaml is pain

## Prerequisites

- OCaml 5.x
- PostgreSQL with `libpq` development headers

## Setup

```bash
opam switch create . 5.4.0 --deps-only
eval $(opam env)
opam install ocaml-lsp-server ocamlformat
dune build
```

## Usage

```bash
dune exec importer
```
