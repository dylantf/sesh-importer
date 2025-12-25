import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

type CsvRow =
  Dict(String, String)

pub type Sport {
  Kiteboarding
  Sup
  Skiing
  Snowboarding
  MountainBiking
  Hiking
  Running
  Paragliding
  Surfing
  WingFoiling
  Parawinging
}

pub type SeshType {
  Spot
  Downwinder
  Roundwinder
}

pub type BoardType {
  Twintip
  Hydrofoil
  Surfboard
  SupBoard
  Skis
  Snowboard
  Other
}

pub type Normalized {
  Normalized(
    date: Time,
    sport: Sport,
    hours: Float,
    wind_avg: Option(Int),
    wind_gust: Option(Int),
    kite_size: Option(List(String)),
    wing_size: Option(List(String)),
    sesh_type: Option(SeshType),
    board_type: Option(List(BoardType)),
    foil: Option(List(String)),
    foil_board: Option(String),
    location: Option(String),
    comments: Option(String),
  )
}

// Helpers

fn maybe_string(input: String) -> Option(String) {
  case string.trim(input) {
    "" -> None
    s -> Some(s)
  }
}

fn parse_int(input: String) -> Option(Int) {
  input
  |> maybe_string
  |> option.then(fn(s) { int.parse(s) |> option.from_result })
}

fn parse_float(input: String) -> Float {
  case float.parse(input) {
    Ok(f) -> f
    Error(_) -> {
      // Try parsing as int and convert to float
      let assert Ok(i) = int.parse(input)
      int.to_float(i)
    }
  }
}

// Converts google sheets M/D/YYYY string to Time type
fn parse_date(input: String) -> Time {
  let assert [m, d, y] = string.split(input, "/")
  let m = string.pad_start(m, to: 2, with: "0")
  let d = string.pad_start(d, to: 2, with: "0")
  let assert Ok(time) =
    birl.from_naive(y <> "-" <> m <> "-" <> d <> " 00:00:00")
  time
}

fn parse_many(input: String) -> Option(List(String)) {
  case maybe_string(input) {
    None -> None
    Some(s) -> Some(string.split(s, ",") |> list.map(string.trim))
  }
}

fn parse_sport(s: String) -> Sport {
  case string.trim(s) {
    "Kiteboarding" -> Kiteboarding
    "SUP" -> Sup
    "Skiing" -> Skiing
    "Snowboarding" -> Snowboarding
    "Mountain Biking" -> MountainBiking
    "Hiking" -> Hiking
    "Running" -> Running
    "Paragliding" -> Paragliding
    "Surfing" | "Surf" -> Surfing
    "Wing foiling" -> WingFoiling
    "Parawinging" -> Parawinging
    _ -> panic as { "Unknown sport: " <> s }
  }
}

fn parse_sesh_type(s: String) -> Option(SeshType) {
  case s {
    "Spot" -> Some(Spot)
    "Downwinder" -> Some(Downwinder)
    "Roundwinder" -> Some(Roundwinder)
    _ -> None
  }
}

fn parse_board_type(type_name: String) -> BoardType {
  case type_name {
    "Twintip" | "Twintp" -> Twintip
    "Hydrofoil" -> Hydrofoil
    "Surfboard" | "Strapless" -> Surfboard
    "SUP" -> SupBoard
    "Skis" -> Skis
    "Snowboard" -> Snowboard
    "Skim" -> Other
    other -> panic as { "Unhandled board type: " <> other }
  }
}

fn parse_board_types(input: Option(String)) -> Option(List(BoardType)) {
  input
  |> option.then(parse_many)
  |> option.map(list.map(_, parse_board_type))
}

// Some kites are defined as "12m" and others just "12". Normalize to just number strings.
fn normalize_kite_sizes(kites: Option(List(String))) -> Option(List(String)) {
  kites
  |> option.map(list.map(_, string.replace(in: _, each: "m", with: "")))
}

fn col(row: CsvRow, key: String) -> String {
  dict.get(row, key) |> option.from_result |> option.unwrap("")
}

fn parse_2012(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Lull (kn)") |> parse_int,
    wind_gust: row |> col("Gust (kn)") |> parse_int,
    kite_size: row |> col("Kite Size") |> parse_many |> normalize_kite_sizes,
    wing_size: None,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: None,
    foil: None,
    foil_board: None,
    location: None,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2013(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Lull") |> parse_int,
    wind_gust: row |> col("Gust") |> parse_int,
    kite_size: row |> col("Kite") |> parse_many |> normalize_kite_sizes,
    wing_size: None,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: None,
    foil: None,
    foil_board: None,
    location: None,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2014(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Day") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Lull (kn)") |> parse_int,
    wind_gust: row |> col("Gust (kn)") |> parse_int,
    kite_size: row |> col("Kite Size") |> parse_many |> normalize_kite_sizes,
    wing_size: None,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: None,
    foil: None,
    foil_board: None,
    location: row |> col("Location") |> maybe_string,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2015(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Lull") |> parse_int,
    wind_gust: row |> col("Gust") |> parse_int,
    kite_size: row |> col("Kite") |> parse_many |> normalize_kite_sizes,
    wing_size: None,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: None,
    foil: None,
    foil_board: None,
    location: row |> col("Location") |> maybe_string,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2016(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Lull (kts)") |> parse_int,
    wind_gust: row |> col("Gust (kts)") |> parse_int,
    kite_size: row |> col("Kite") |> parse_many |> normalize_kite_sizes,
    wing_size: None,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: row |> col("Board") |> maybe_string |> parse_board_types,
    foil: None,
    foil_board: None,
    location: row |> col("Location") |> maybe_string,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2022(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Avg (kts)") |> parse_int,
    wind_gust: row |> col("Gust (kts)") |> parse_int,
    kite_size: row |> col("Kite") |> parse_many |> normalize_kite_sizes,
    wing_size: row |> col("Wing") |> parse_many,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: row |> col("Board Type") |> maybe_string |> parse_board_types,
    foil: row |> col("Foil") |> parse_many,
    foil_board: row |> col("Foil Board") |> maybe_string,
    location: row |> col("Location") |> maybe_string,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_2025(row: CsvRow) -> Normalized {
  Normalized(
    date: row |> col("Date") |> parse_date,
    sport: row |> col("Sport") |> parse_sport,
    hours: row |> col("Hours") |> parse_float,
    wind_avg: row |> col("Avg (kts)") |> parse_int,
    wind_gust: row |> col("Gust (kts)") |> parse_int,
    kite_size: row |> col("Kite") |> parse_many |> normalize_kite_sizes,
    wing_size: row |> col("Wing") |> parse_many,
    sesh_type: row |> col("Type") |> parse_sesh_type,
    board_type: row |> col("Board Type") |> maybe_string |> parse_board_types,
    foil: row |> col("Foil") |> parse_many,
    foil_board: row |> col("Board") |> maybe_string,
    location: row |> col("Location") |> maybe_string,
    comments: row |> col("Comments") |> maybe_string,
  )
}

fn parse_row(row: CsvRow, year: Int) -> Normalized {
  case year {
    2012 -> parse_2012(row)
    2013 -> parse_2013(row)
    2014 -> parse_2014(row)
    2015 -> parse_2015(row)
    2016 | 2017 | 2018 | 2019 | 2020 | 2021 -> parse_2016(row)
    2022 | 2023 | 2024 -> parse_2022(row)
    2025 -> parse_2025(row)
    _ -> panic as { "Not implemented: " <> int.to_string(year) }
  }
}

pub fn parse_file(year: Int, rows: List(CsvRow)) -> List(Normalized) {
  list.map(rows, parse_row(_, year))
}
