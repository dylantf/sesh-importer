# OCaml is pain

## Prerequisites

- OCaml 5.x
- PostgreSQL with `libpq` development headers

## Setup

```bash
opam install dune csv caqti caqti-lwt caqti-driver-postgresql lwt uri

dune build
```

## Usage

```bash
dune exec importer
```
