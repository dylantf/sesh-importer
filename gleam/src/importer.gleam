import birl
import gear
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gsv
import parsers.{type Normalized}
import simplifile

fn import_path(year: Int) -> String {
  "/home/dylan/Desktop/Sesh Import/" <> int.to_string(year) <> ".csv"
}

fn read_file(year: Int) -> List(Dict(String, String)) {
  let path = import_path(year)
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(rows) = gsv.to_dicts(contents, separator: ",")
  // Drop header row
  list.drop(rows, 1)
}

fn gear_ids(sesh: Normalized) -> List(Int) {
  list.flatten([
    gear.kite_ids(sesh),
    gear.foil_ids(sesh),
    gear.board_ids(sesh),
    gear.wing_ids(sesh),
  ])
}

fn debug_sesh(sesh: Normalized) {
  let date = birl.to_iso8601(sesh.date)
  let ids =
    gear_ids(sesh)
    |> list.map(int.to_string)
    |> string.join(", ")
  io.println(date <> " [" <> ids <> "]")
}

pub fn main() {
  let years = list.range(2012, 2025)

  let normalized =
    years
    |> list.map(fn(year) {
      let rows = read_file(year)
      parsers.parse_file(year, rows)
    })
    |> list.flatten

  normalized |> list.each(debug_sesh)
}
