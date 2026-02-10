# Importer

### Prerequisites:

- GHC 9.6.7
- Cabal 3.8
- Postgresql 17
- pg_config on path (`/usr/pgsql-17/bin`) (comes from libpq-devel)
- CSV files at correct location with correct names (see app/Main.hs)

- For Fedora: `sudo dnf install libpq-devel libpqxx-devel`

### Building:

- `cabal build`

### Running:

- `cabal run`
