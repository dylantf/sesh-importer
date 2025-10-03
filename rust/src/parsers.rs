use chrono::NaiveDate;
use csv::Reader;
use serde::Deserialize;
use std::fs::File;

#[derive(Debug, PartialEq)]
pub enum Sport {
    Kiteboarding,
    Sup,
    Skiing,
    Snowboarding,
    MountainBiking,
    Hiking,
    Running,
    Paragliding,
    Surfing,
    WingFoiling,
    Parawinging,
}

#[derive(Debug)]
pub enum SeshType {
    Spot,
    Downwinder,
    Roundwinder,
}

#[derive(Debug, PartialEq)]
pub enum BoardType {
    Twintip,
    Hydrofoil,
    Surfboard,
    Sup,
    Skis,
    Snowboard,
    Other,
}

#[derive(Debug)]
pub struct Normalized {
    pub date: NaiveDate,
    pub sport: Sport,
    pub hours: f32,
    pub wind_avg: Option<i32>,
    pub wind_gust: Option<i32>,
    pub kite_size: Option<Vec<String>>,
    pub wing_size: Option<Vec<String>>,
    pub sesh_type: Option<SeshType>,
    pub board_type: Option<Vec<BoardType>>,
    pub foil: Option<Vec<String>>,
    pub foil_board: Option<String>,
    pub location: Option<String>,
    pub comments: Option<String>,
}

fn maybe_string(s: String) -> Option<String> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

fn parse_many(s: String) -> Option<Vec<String>> {
    maybe_string(s).map(|s| s.split(',').map(|item| item.trim().to_string()).collect())
}

fn parse_sport(s: &str) -> Sport {
    use Sport::*;
    match s {
        "Kiteboarding" => Kiteboarding,
        "SUP" => Sup,
        "Skiing" => Skiing,
        "Snowboarding" => Snowboarding,
        "Mountain Biking" => MountainBiking,
        "Hiking" => Hiking,
        "Running" => Running,
        "Paragliding" => Paragliding,
        "Surfing" => Surfing,
        "Surf" => Surfing,
        "Wing foiling" => WingFoiling,
        "Parawinging" => Parawinging,
        _ => panic!("Unknown sport: {}", s),
    }
}

fn parse_sesh_type(s: &str) -> Option<SeshType> {
    use SeshType::*;
    match s {
        "Spot" => Some(Spot),
        "Downwinder" => Some(Downwinder),
        "Roundwinder" => Some(Roundwinder),
        _ => None,
    }
}

fn parse_board_type(type_name: &str) -> BoardType {
    use BoardType::*;
    match type_name {
        "Twintip" | "Twintp" => Twintip,
        "Hydrofoil" => Hydrofoil,
        "Surfboard" | "Strapless" => Surfboard,
        "SUP" => Sup,
        "Skis" => Skis,
        "Snowboard" => Snowboard,
        "Skim" => Other,
        other => panic!("Unhandled board type: {}", other),
    }
}

fn parse_board_types(input: Option<String>) -> Option<Vec<BoardType>> {
    input.and_then(parse_many).map(|board_types| {
        board_types
            .into_iter()
            .map(|bt| parse_board_type(&bt))
            .collect()
    })
}

// Some kites are defined as "12m" and others just "12". Normalize to just number strings.
fn normalize_kite_sizes(kites: Option<Vec<String>>) -> Option<Vec<String>> {
    kites.map(|kites| kites.iter().map(|kite| kite.replace("m", "")).collect())
}

// Need a custom parser format because NaiveDate doesn't like US date strings
mod parse_date {
    use chrono::NaiveDate;
    use serde::{self, Deserialize, Deserializer};

    const FORMAT: &str = "%m/%d/%Y";

    pub fn deserialize<'de, D>(deserializer: D) -> Result<NaiveDate, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let date = NaiveDate::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)?;
        Ok(date)
    }
}

#[derive(Deserialize)]
struct Schema2012 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Lull (kn)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kn)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite Size"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2012(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2012 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: None,
                foil: None,
                foil_board: None,
                location: None,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2013 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Lull"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2013(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2013 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: None,
                foil: None,
                foil_board: None,
                location: None,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2014 {
    #[serde(rename(deserialize = "Day"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Lull (kn)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kn)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite Size"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Location"))]
    location: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2014(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2014 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: None,
                foil: None,
                foil_board: None,
                location: record.location,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2016 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Lull (kts)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kts)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Board"))]
    board_type: Option<String>,

    #[serde(rename(deserialize = "Location"))]
    location: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2016(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2016 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: parse_board_types(record.board_type),
                foil: None,
                foil_board: None,
                location: record.location,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2022 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Avg (kts)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kts)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Board Type"))]
    board_type: Option<String>,

    #[serde(rename(deserialize = "Foil"))]
    foil: Option<String>,

    #[serde(rename(deserialize = "Foil Board"))]
    foil_board: Option<String>,

    #[serde(rename(deserialize = "Location"))]
    location: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2022(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2022 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: parse_board_types(record.board_type),
                foil: record.foil.and_then(parse_many),
                foil_board: record.foil_board,
                location: record.location,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2024 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Avg (kts)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kts)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Wing"))]
    wing_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Board Type"))]
    board_type: Option<String>,

    #[serde(rename(deserialize = "Foil"))]
    foil: Option<String>,

    #[serde(rename(deserialize = "Foil Board"))]
    foil_board: Option<String>,

    #[serde(rename(deserialize = "Location"))]
    location: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2024(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2024 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: record.wing_size.and_then(parse_many),
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: parse_board_types(record.board_type),
                foil: record.foil.and_then(parse_many),
                foil_board: record.foil_board,
                location: record.location,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

#[derive(Deserialize)]
struct Schema2025 {
    #[serde(rename(deserialize = "Date"), with = "parse_date")]
    date: NaiveDate,

    #[serde(rename(deserialize = "Sport"))]
    sport: String,

    #[serde(rename(deserialize = "Hours"))]
    hours: f32,

    #[serde(rename(deserialize = "Avg (kts)"))]
    wind_avg: Option<i32>,

    #[serde(rename(deserialize = "Gust (kts)"))]
    wind_gust: Option<i32>,

    #[serde(rename(deserialize = "Kite"))]
    kite_size: Option<String>,

    #[serde(rename(deserialize = "Wing"))]
    wing_size: Option<String>,

    #[serde(rename(deserialize = "Type"))]
    sesh_type: Option<String>,

    #[serde(rename(deserialize = "Board Type"))]
    board_type: Option<String>,

    #[serde(rename(deserialize = "Foil"))]
    foil: Option<String>,

    #[serde(rename(deserialize = "Board"))]
    foil_board: Option<String>,

    #[serde(rename(deserialize = "Location"))]
    location: Option<String>,

    #[serde(rename(deserialize = "Comments"))]
    comments: Option<String>,
}

fn parse_2025(mut reader: Reader<File>) -> Vec<Normalized> {
    reader
        .deserialize()
        .map(|row| {
            let record: Schema2025 = row.unwrap();
            Normalized {
                date: record.date,
                sport: parse_sport(&record.sport),
                hours: record.hours,
                wind_avg: record.wind_avg,
                wind_gust: record.wind_gust,
                kite_size: record
                    .kite_size
                    .map(parse_many)
                    .and_then(normalize_kite_sizes),
                wing_size: record.wing_size.and_then(parse_many),
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: parse_board_types(record.board_type),
                foil: record.foil.and_then(parse_many),
                foil_board: record.foil_board,
                location: record.location,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

pub fn parse_file(year: &i32, path: &str) -> Vec<Normalized> {
    let reader = csv::ReaderBuilder::new().from_path(path).unwrap();

    match year {
        2012 => parse_2012(reader),
        2013 | 2015 => parse_2013(reader),
        2014 => parse_2014(reader),
        2016..=2021 => parse_2016(reader),
        2022..=2023 => parse_2022(reader),
        2024 => parse_2024(reader),
        2025 => parse_2025(reader),
        _ => panic!("Parser not implemented for year {}", year),
    }
}
