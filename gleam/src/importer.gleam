import db
import envoy
import filepath
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gsv
import parsers
import simplifile

fn import_path(year: Int) -> String {
  let assert Ok(home) = envoy.get("HOME")
  home
  |> filepath.join("Desktop")
  |> filepath.join("Sesh Import")
  |> filepath.join(int.to_string(year) <> ".csv")
}

fn read_file(year: Int) -> List(Dict(String, String)) {
  let path = import_path(year)
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(rows) = gsv.to_dicts(contents, separator: ",")
  rows
}

pub fn main() {
  let years = list.range(2012, 2026)

  let normalized =
    years
    |> list.map(fn(year) {
      let rows = read_file(year)
      parsers.parse_file(year, rows)
    })
    |> list.flatten

  db.import_all(normalized)
}
