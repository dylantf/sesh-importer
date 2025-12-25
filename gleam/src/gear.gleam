import birl.{type Time}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import parsers.{
  type BoardType, type Normalized, type Sport, Hydrofoil, Parawinging, Skis,
  Snowboard, Surfboard, Twintip, WingFoiling,
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
  let assert Ok(time) = birl.parse(d)
  time
}

fn before(date_str: String, sesh_date: Time) -> Bool {
  let ord =
    date_str
    |> parse_date
    |> birl.compare(sesh_date)

  ord == order.Lt
}

fn after(date_str: String, sesh_date: Time) -> Bool {
  let ord =
    date_str
    |> parse_date
    |> birl.compare(sesh_date)

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
    GearRule("", before("2019-05-26", sesh_date), 17),
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

// Pattern matching in gleam kind of sucks ??? why can't we just have functions in guards

type BoardRule {
  BoardRule(board_type: BoardType, board_name: String, condition: Bool, id: Int)
}

fn derive_board_rules(
  rules: List(BoardRule),
  board_type: BoardType,
  board_name: String,
) -> Option(Int) {
  rules
  |> list.find(fn(rule) {
    rule.board_type == board_type
    && rule.board_name == board_name
    && rule.condition
  })
  |> option.from_result
  |> option.map(fn(rule) { rule.id })
}

fn derive_board_id(
  sesh_date: Time,
  board_type: BoardType,
  board_name: Option(String),
) -> Option(Int) {
  let name = option.unwrap(board_name, "")
  [
    // Hydrofoil boards
    BoardRule(Hydrofoil, "Groove Skate", after("2022-08-10", sesh_date), 31),
    BoardRule(Hydrofoil, "", after("2022-08-10", sesh_date), 31),
    BoardRule(Hydrofoil, "Rocket v2 85L", True, 32),
    BoardRule(Hydrofoil, "Rocket v2 60L", True, 34),
    BoardRule(Hydrofoil, "Rocket 60L", True, 34),
    BoardRule(Hydrofoil, "Flying Fish 40L", True, 33),
    BoardRule(Hydrofoil, "LF Galaxy", True, 30),
    BoardRule(Hydrofoil, "", between("2017-06-24", "2022-08-09", sesh_date), 30),
    BoardRule(Hydrofoil, "", before("2017-06-23", sesh_date), 17),
    // Other board types
    BoardRule(Surfboard, "", True, 43),
    BoardRule(Twintip, "", True, 44),
    BoardRule(Skis, "", True, 45),
    BoardRule(Snowboard, "", True, 46),
  ]
  |> derive_board_rules(board_type, name)
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

type WingRule {
  WingRule(sport: Sport, size: String, condition: Bool, id: Int)
}

fn derive_wing_rules(
  rules: List(WingRule),
  sport: Sport,
  size: String,
) -> Option(Int) {
  rules
  |> list.find(fn(rule) {
    rule.sport == sport && rule.size == size && rule.condition
  })
  |> option.from_result
  |> option.map(fn(rule) { rule.id })
}

fn derive_wing_id(sport: Sport, sesh_date: Time, size: String) -> Option(Int) {
  [
    WingRule(WingFoiling, "6m", True, 35),
    WingRule(WingFoiling, "5m", before("2024-01-01", sesh_date), 36),
    WingRule(WingFoiling, "5m", after("2024-01-01", sesh_date), 39),
    WingRule(WingFoiling, "4m", before("2024-01-01", sesh_date), 37),
    WingRule(WingFoiling, "4m", after("2024-01-01", sesh_date), 40),
    WingRule(WingFoiling, "5.5m", True, 38),
    WingRule(WingFoiling, "3m", True, 41),
    WingRule(Parawinging, "4m", True, 42),
  ]
  |> derive_wing_rules(sport, size)
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
