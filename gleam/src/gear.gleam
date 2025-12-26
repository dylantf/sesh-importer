import birl.{type Time}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import parsers.{
  type BoardType, type Normalized, type Sport, Hydrofoil, Other, Parawinging,
  Skis, Snowboard, SupBoard, Surfboard, Twintip, WingFoiling,
}

type GearRule {
  GearRule(input: String, condition: Bool, id: Int)
}

fn derive_gear_id(rules: List(GearRule), input: String) -> Option(Int) {
  rules
  |> list.find(fn(rule) { rule.input == input && rule.condition })
  |> option.from_result
  |> option.map(fn(rule) { rule.id })
}

fn parse_date(d: String) -> Time {
  let assert Ok(time) = birl.from_naive(d <> " 00:00:00")
  time
}

// sesh_date is on or before the given date
fn before(date_str: String, sesh_date: Time) -> Bool {
  let rule_date = parse_date(date_str)
  let ord = birl.compare(sesh_date, rule_date)
  ord == order.Lt || ord == order.Eq
}

// sesh_date is on or after the given date
fn after(date_str: String, sesh_date: Time) -> Bool {
  let rule_date = parse_date(date_str)
  let ord = birl.compare(sesh_date, rule_date)
  ord == order.Gt || ord == order.Eq
}

fn between(start: String, end: String, sesh_date: Time) -> Bool {
  after(start, sesh_date) && before(end, sesh_date)
}

fn derive_kite_id(kite_size: String, sesh_date: Time) -> Option(Int) {
  [
    GearRule("12", before("2013-01-15", sesh_date), 1),
    GearRule("8", before("2013-01-15", sesh_date), 2),
    GearRule("12", between("2013-01-16", "2013-12-07", sesh_date), 3),
    GearRule("9", between("2013-01-16", "2013-12-07", sesh_date), 4),
    GearRule("13", between("2013-12-08", "2014-12-31", sesh_date), 6),
    GearRule("9", between("2013-12-08", "2014-12-31", sesh_date), 7),
    GearRule("7", between("2013-12-08", "2014-12-31", sesh_date), 8),
    GearRule("12", between("2015-01-01", "2017-02-05", sesh_date), 9),
    GearRule("10", between("2016-10-09", "2018-01-24", sesh_date), 10),
    GearRule("7", between("2016-10-09", "2019-10-04", sesh_date), 11),
    GearRule("12", between("2017-02-06", "2022-08-10", sesh_date), 12),
    GearRule("10", between("2018-01-25", "2020-12-31", sesh_date), 13),
    GearRule("7", between("2019-10-05", "2020-12-31", sesh_date), 14),
    GearRule("9", after("2021-01-01", sesh_date), 15),
    GearRule("7", after("2021-01-01", sesh_date), 16),
  ]
  |> derive_gear_id(kite_size)
}

fn derive_foil_id(sesh_date: Time, foil_name: Option(String)) -> Option(Int) {
  let name = option.unwrap(foil_name, "")
  [
    GearRule("", before("2019-05-27", sesh_date), 17),
    GearRule("", after("2019-05-27", sesh_date), 18),
    GearRule("Thruster", True, 18),
    GearRule("ART 999", True, 19),
    GearRule("ART 799", True, 20),
    GearRule("Phantom 1480", True, 21),
    GearRule("Seven Seas 1200", True, 22),
    GearRule("Phantom-S 840", True, 23),
    GearRule("Eagle 890", True, 24),
    GearRule("Eagle 990", True, 25),
    GearRule("Ypra-S 785", True, 26),
    GearRule("Ypra-S 1000", True, 27),
    GearRule("Veloce 890", True, 28),
  ]
  |> derive_gear_id(name)
}

pub fn kite_ids(sesh: Normalized) -> List(Int) {
  case sesh.kite_size {
    None -> []
    Some(kites) ->
      kites
      |> list.map(derive_kite_id(_, sesh.date))
      |> option.values
  }
}

fn is_foil_sesh(sesh: Normalized) -> Bool {
  case sesh.board_type {
    Some(bt) -> list.contains(bt, Hydrofoil)
    None -> False
  }
}

pub fn foil_ids(sesh: Normalized) -> List(Int) {
  case is_foil_sesh(sesh), sesh.foil {
    True, None -> [derive_foil_id(sesh.date, None)]
    True, Some(foil_names) ->
      foil_names
      |> list.map(fn(foil) { derive_foil_id(sesh.date, Some(foil)) })
    _, _ -> []
  }
  |> option.values
}

fn derive_board_id(
  sesh_date: Time,
  board_type: BoardType,
  board_name: Option(String),
) -> Option(Int) {
  case board_type {
    Hydrofoil -> {
      let name = option.unwrap(board_name, "")
      [
        GearRule("Groove Skate", after("2022-08-10", sesh_date), 31),
        GearRule("", after("2022-08-10", sesh_date), 31),
        GearRule("Rocket v2 85L", True, 32),
        GearRule("Rocket v2 60L", True, 34),
        GearRule("Rocket 60L", True, 34),
        GearRule("Flying Fish 40L", True, 33),
        GearRule("LF Galaxy", True, 30),
        GearRule("", between("2017-06-24", "2022-08-09", sesh_date), 30),
        GearRule("", before("2017-06-23", sesh_date), 17),
      ]
      |> derive_gear_id(name)
    }
    Surfboard -> Some(43)
    Twintip -> Some(44)
    Skis -> Some(45)
    Snowboard -> Some(46)
    SupBoard | Other -> None
  }
}

pub fn board_ids(sesh: Normalized) -> List(Int) {
  case sesh.board_type {
    Some(board_types) ->
      board_types
      |> list.map(fn(bt) { derive_board_id(sesh.date, bt, sesh.foil_board) })
      |> option.values
    None -> []
  }
}

fn derive_wing_id(sport: Sport, sesh_date: Time, size: String) -> Option(Int) {
  let wings = [
    GearRule("6m", True, 35),
    GearRule("5m", before("2024-01-01", sesh_date), 36),
    GearRule("5m", after("2024-01-01", sesh_date), 39),
    GearRule("4m", before("2024-01-01", sesh_date), 37),
    GearRule("4m", after("2024-01-01", sesh_date), 40),
    GearRule("5.5m", True, 38),
    GearRule("3m", True, 41),
  ]

  let parawings = [GearRule("4m", True, 42)]

  case sport {
    WingFoiling -> wings
    Parawinging -> parawings
    _ -> []
  }
  |> derive_gear_id(size)
}

fn is_wing_sport(sport: Sport) -> Bool {
  sport == WingFoiling || sport == Parawinging
}

pub fn wing_ids(sesh: Normalized) -> List(Int) {
  case is_wing_sport(sesh.sport), sesh.wing_size {
    True, Some(sizes) ->
      sizes
      |> list.map(fn(sz) { derive_wing_id(sesh.sport, sesh.date, sz) })
      |> option.values
    _, _ -> []
  }
}
