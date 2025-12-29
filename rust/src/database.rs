use chrono::NaiveDate;
use sqlx::{PgPool, QueryBuilder, Row, postgres::PgPoolOptions};

use crate::{
    gear::{board_ids, foil_ids, kite_ids, wing_ids},
    parsers::{Normalized, SeshType, Sport},
};

fn stringify_sport(sport: &Sport) -> String {
    use Sport::*;
    match sport {
        Kiteboarding => "kiteboarding",
        Sup => "sup",
        Skiing => "skiing",
        Snowboarding => "snowboarding",
        MountainBiking => "mountain biking",
        Hiking => "hiking",
        Running => "running",
        Paragliding => "paragliding",
        Surfing => "surfing",
        WingFoiling => "wing foiling",
        Parawinging => "parawinging",
    }
    .to_string()
}

fn stringify_sesh_type(sesh_type: &SeshType) -> String {
    use SeshType::*;
    match sesh_type {
        Spot => "spot",
        Downwinder => "downwinder",
        Roundwinder => "roundwinder",
    }
    .to_string()
}

fn hours_to_seconds(hours: f32) -> i32 {
    let seconds = hours * 60.0 * 60.0;
    seconds.ceil() as i32
}

fn is_wind_sport(sport: &Sport) -> bool {
    use Sport::*;
    matches!(sport, Kiteboarding | WingFoiling | Parawinging)
}

fn get_gear_ids(normalized: &Normalized) -> Vec<i32> {
    [
        kite_ids(normalized),
        wing_ids(normalized),
        board_ids(normalized),
        foil_ids(normalized),
    ]
    .concat()
}

struct WindDetails {
    wind_avg: i32,
    wind_gust: i32,
    sesh_type: String,
}

impl WindDetails {
    fn from_normalized(n: &Normalized) -> Option<Self> {
        match (
            is_wind_sport(&n.sport),
            n.wind_avg,
            n.wind_gust,
            &n.sesh_type,
        ) {
            (true, Some(avg), Some(gust), Some(st)) => Some(Self {
                wind_avg: avg,
                wind_gust: gust,
                sesh_type: stringify_sesh_type(st),
            }),
            (true, _, _, _) => panic!("Wind sport missing wind data: {:?}", n),
            (false, _, _, _) => None,
        }
    }
}

struct MappedSesh {
    user_id: i32,
    date: NaiveDate,
    sport: String,
    duration_seconds: i32,
    location: Option<String>,
    comments: Option<String>,
    wind_details: Option<WindDetails>,
    gear_ids: Vec<i32>,
}

impl From<&Normalized> for MappedSesh {
    fn from(n: &Normalized) -> Self {
        Self {
            user_id: 1,
            date: n.date,
            sport: stringify_sport(&n.sport),
            duration_seconds: hours_to_seconds(n.hours),
            location: n.location.clone(),
            comments: n.comments.clone(),
            wind_details: WindDetails::from_normalized(n),
            gear_ids: get_gear_ids(n),
        }
    }
}

async fn insert_seshes<'a>(
    pool: &PgPool,
    mapped: &'a [MappedSesh],
) -> Result<Vec<(i32, &'a MappedSesh)>, sqlx::Error> {
    let mut qb = QueryBuilder::new(
        "INSERT INTO seshes (user_id, date, sport, duration_seconds, location_name, comments) ",
    );

    qb.push_values(mapped.iter(), |mut b, m| {
        b.push_bind(m.user_id)
            .push_bind(m.date)
            .push_bind(&m.sport)
            .push_bind(m.duration_seconds)
            .push_bind(&m.location)
            .push_bind(&m.comments);
    });
    qb.push(" RETURNING id");

    let rows = qb.build().fetch_all(pool).await?;
    let with_ids: Vec<_> = mapped
        .iter()
        .zip(rows.iter())
        .map(|(m, row)| (row.get("id"), m))
        .collect();

    println!("Inserted {} seshes", mapped.len());
    Ok(with_ids)
}

async fn insert_wind_details(
    pool: &PgPool,
    with_ids: &[(i32, &MappedSesh)],
) -> Result<(), sqlx::Error> {
    let wind_records: Vec<_> = with_ids
        .iter()
        .filter_map(|(sesh_id, m)| {
            m.wind_details
                .as_ref()
                .map(|wd| (*sesh_id, wd.wind_avg, wd.wind_gust, &wd.sesh_type))
        })
        .collect();

    if wind_records.is_empty() {
        return Ok(());
    }

    let mut qb = QueryBuilder::new(
        "INSERT INTO wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type) ",
    );

    qb.push_values(&wind_records, |mut b, (sesh_id, avg, gust, st)| {
        b.push_bind(sesh_id)
            .push_bind(avg)
            .push_bind(gust)
            .push_bind(st);
    });

    qb.build().execute(pool).await?;
    println!("Inserted {} wind details", wind_records.len());
    Ok(())
}

async fn insert_gear(pool: &PgPool, with_ids: &[(i32, &MappedSesh)]) -> Result<(), sqlx::Error> {
    let gear_records: Vec<_> = with_ids
        .iter()
        .flat_map(|(sesh_id, m)| m.gear_ids.iter().map(move |&gear_id| (*sesh_id, gear_id)))
        .collect();

    if gear_records.is_empty() {
        return Ok(());
    }

    let mut qb = QueryBuilder::new("INSERT INTO sesh_gear (sesh_id, gear_id) ");

    qb.push_values(&gear_records, |mut b, (sesh_id, gear_id)| {
        b.push_bind(sesh_id).push_bind(gear_id);
    });

    qb.build().execute(pool).await?;
    println!("Inserted {} gear records", gear_records.len());
    Ok(())
}

pub async fn insert_records(normalized: Vec<Normalized>) -> Result<(), sqlx::Error> {
    let pool = PgPoolOptions::new()
        .max_connections(1)
        .connect("postgres://dylan:dylan@localhost:5432/seshtracker_dev")
        .await?;

    let mapped: Vec<MappedSesh> = normalized.iter().map(|n| n.into()).collect();
    let with_ids = insert_seshes(&pool, &mapped).await?;

    insert_wind_details(&pool, &with_ids).await?;
    insert_gear(&pool, &with_ids).await?;

    Ok(())
}
