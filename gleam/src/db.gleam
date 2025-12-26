import birl
import cake/adapter/postgres
import cake/insert as i
import cake/internal/write_query.{InsertParam}
import cake/param
import gear
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleam/time/calendar
import parsers.{
  type Normalized, type SeshType, type Sport, Downwinder, Hiking, Kiteboarding,
  MountainBiking, Paragliding, Parawinging, Roundwinder, Running, Skiing,
  Snowboarding, Spot, Sup, Surfing, WingFoiling,
}

fn stringify_sport(sport: Sport) -> String {
  case sport {
    Kiteboarding -> "kiteboarding"
    Sup -> "sup"
    Skiing -> "skiing"
    Snowboarding -> "snowboarding"
    MountainBiking -> "mountain biking"
    Hiking -> "hiking"
    Running -> "running"
    Paragliding -> "paragliding"
    Surfing -> "surfing"
    WingFoiling -> "wing foiling"
    Parawinging -> "parawinging"
  }
}

fn stringify_sesh_type(sesh_type: SeshType) -> String {
  case sesh_type {
    Spot -> "spot"
    Downwinder -> "downwinder"
    Roundwinder -> "roundwinder"
  }
}

fn hours_to_seconds(hours: Float) -> Int {
  float.round(hours *. 60.0 *. 60.0)
}

fn is_wind_sport(sport: Sport) -> Bool {
  case sport {
    Kiteboarding | WingFoiling | Parawinging -> True
    _ -> False
  }
}

fn date_value(time: birl.Time) -> i.InsertValue {
  let day = birl.get_day(time)
  let assert Ok(month) = calendar.month_from_int(day.month)
  let date = calendar.Date(year: day.year, month: month, day: day.date)
  InsertParam(param.date(date))
}

fn sesh_query(n: Normalized) {
  let sport = stringify_sport(n.sport)
  let duration = hours_to_seconds(n.hours)

  let row =
    [
      i.int(1),
      date_value(n.date),
      i.string(sport),
      i.int(duration),
      case n.location {
        Some(loc) -> i.string(loc)
        None -> i.null()
      },
      case n.comments {
        Some(c) -> i.string(c)
        None -> i.null()
      },
    ]
    |> i.row

  [row]
  |> i.from_values(table_name: "seshes", columns: [
    "user_id",
    "date",
    "sport",
    "duration_seconds",
    "location_name",
    "comments",
  ])
  |> i.returning(["id"])
  |> i.to_query
}

fn wind_details_query(n: Normalized, sesh_id: Int) {
  let assert Some(wind_avg) = n.wind_avg
  let assert Some(wind_gust) = n.wind_gust
  let assert Some(sesh_type) = n.sesh_type

  let row =
    [
      i.int(sesh_id),
      i.int(wind_avg),
      i.int(wind_gust),
      i.string(stringify_sesh_type(sesh_type)),
    ]
    |> i.row

  [row]
  |> i.from_values(table_name: "wind_sesh_details", columns: [
    "sesh_id",
    "wind_avg",
    "wind_gust",
    "sesh_type",
  ])
  |> i.to_query
}

fn gear_query(sesh_id: Int, gear_ids: List(Int)) {
  let rows =
    gear_ids
    |> list.map(fn(gear_id) { i.row([i.int(sesh_id), i.int(gear_id)]) })

  rows
  |> i.from_values(table_name: "sesh_gear", columns: ["sesh_id", "gear_id"])
  |> i.to_query
}

fn gear_ids(n: Normalized) -> List(Int) {
  list.flatten([
    gear.kite_ids(n),
    gear.wing_ids(n),
    gear.board_ids(n),
    gear.foil_ids(n),
  ])
}

pub fn import_all(normalized: List(Normalized)) -> Nil {
  use conn <- postgres.with_connection(
    host: "localhost",
    port: 5432,
    username: "dylan",
    password: Some("dylan"),
    database: "seshtracker_dev",
  )

  list.each(normalized, fn(n) {
    // Insert sesh and get id
    let decoder = decode.at([0], decode.int)
    let assert Ok([sesh_id]) =
      sesh_query(n) |> postgres.run_write_query(decoder, conn)

    let date = birl.to_date_string(n.date)
    let sport = stringify_sport(n.sport)
    io.println(
      "Inserted sesh: " <> int.to_string(sesh_id) <> " " <> date <> " " <> sport,
    )

    // Insert wind details if applicable
    case is_wind_sport(n.sport), n.wind_avg, n.wind_gust, n.sesh_type {
      True, Some(_), Some(_), Some(_) -> {
        let assert Ok(_) =
          wind_details_query(n, sesh_id)
          |> postgres.run_write_query(decode.dynamic, conn)
        io.println("-- Inserted wind sesh details")
      }
      _, _, _, _ -> Nil
    }

    // Insert gear
    case gear_ids(n) {
      [] -> Nil
      ids -> {
        let assert Ok(_) =
          gear_query(sesh_id, ids)
          |> postgres.run_write_query(decode.dynamic, conn)
        io.println(
          "-- Inserted gear: "
          <> string.join(list.map(ids, int.to_string), ", "),
        )
      }
    }
  })
}
