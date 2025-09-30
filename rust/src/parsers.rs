use chrono::NaiveDate;
use csv::Reader;
use serde::Deserialize;
use std::fs::File;

#[derive(Debug)]
enum Sport {
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
enum SeshType {
    Spot,
    Downwinder,
    Roundwinder,
}

#[derive(Debug)]
enum BoardType {
    Twintip,
    Hydrofoil,
    Surfboard,
    Sup,
    Skis,
    Snowboard,
    Other,
}

#[derive(Debug)]
struct Normalized {
    date: NaiveDate,
    sport: Sport,
    hours: f32,
    wind_avg: Option<u32>,
    wind_gust: Option<u32>,
    kite_size: Option<Vec<String>>,
    wing_size: Option<Vec<String>>,
    sesh_type: Option<SeshType>,
    board_type: Option<Vec<BoardType>>,
    foil: Option<Vec<String>>,
    board: Option<Vec<String>>,
    location: Option<String>,
    comments: Option<String>,
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

fn parse_board_type(s: &str) -> BoardType {
    use BoardType::*;
    match s {
        "Twintip" | "Twintp" => Twintip,
        "Hydrofoil" => Hydrofoil,
        "Surfboard" | "Strapless" => Surfboard,
        "SUP" => Sup,
        "Skis" => Skis,
        "Snowboard" => Snowboard,
        "Skim" => Other,
        _ => panic!("Unhandled board type: {}", s),
    }
}

// Some kites are defined as "12m" and others just "12". Normalize to just number strings.
fn normalize_kite_size(kites: Option<Vec<String>>) -> Option<Vec<String>> {
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
    wind_avg: Option<u32>,

    #[serde(rename(deserialize = "Gust (kn)"))]
    wind_gust: Option<u32>,

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
                kite_size: record.kite_size.and_then(parse_many),
                wing_size: None,
                sesh_type: record.sesh_type.as_deref().and_then(parse_sesh_type),
                board_type: None,
                foil: None,
                board: None,
                location: None,
                comments: record.comments,
            }
        })
        .collect::<Vec<Normalized>>()
}

pub fn test_stuff() {
    let reader = csv::ReaderBuilder::new()
        .from_path("/home/dylan/Desktop/Sesh Import/2012.csv")
        .unwrap();

    let parsed = parse_2012(reader);
    println!("Parsed: {:?}", parsed)
}
