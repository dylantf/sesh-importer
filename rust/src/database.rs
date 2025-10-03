use sqlx::{PgPool, QueryBuilder, Row, postgres::PgPoolOptions};

use crate::{
    gear::{board_ids, foil_ids, kite_ids, wing_ids},
    parsers::{Normalized, SeshType, Sport},
};

fn stringify_sport(sport: &Sport) -> &str {
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
}

fn stringify_sesh_type(sesh_type: &SeshType) -> &str {
    use SeshType::*;
    match sesh_type {
        Spot => "spot",
        Downwinder => "downwinder",
        Roundwinder => "roundwinder",
    }
}

fn hours_to_seconds(hours: f32) -> i32 {
    let seconds = hours * 60.0 * 60.0;
    seconds.ceil() as i32
}

async fn insert_sesh(pool: &PgPool, sesh: &Normalized) -> Result<i32, sqlx::Error> {
    let query = sqlx::query_as::<_, (i32,)>(
        r#"
        insert into seshes 
        (user_id, date, sport, duration_seconds, location_name, comments) 
        values ($1, $2, $3, $4, $5, $6) 
        returning id
        "#,
    )
    .bind(1i32)
    .bind(sesh.date)
    .bind(stringify_sport(&sesh.sport))
    .bind(hours_to_seconds(sesh.hours))
    .bind(&sesh.location)
    .bind(&sesh.comments);

    let id = query.fetch_one(pool).await?.0;
    Ok(id)
}

async fn insert_wind_sesh_details(
    pool: &PgPool,
    sesh_id: i32,
    normalized: &Normalized,
) -> Result<i32, sqlx::Error> {
    match normalized {
        Normalized {
            wind_avg: Some(wind_avg),
            wind_gust: Some(wind_gust),
            sesh_type: Some(sesh_type),
            ..
        } => {
            let query = sqlx::query_as::<_, (i32,)>(
                r#"
                insert into wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type)
                values ($1, $2, $3, $4)
                returning id
                "#,
            )
            .bind(sesh_id)
            .bind(wind_avg)
            .bind(wind_gust)
            .bind(stringify_sesh_type(sesh_type));

            let id = query.fetch_one(pool).await?.0;
            Ok(id)
        }
        _ => panic!("Invalid wind sesh details!"),
    }
}

fn is_wind_sport(normalized: &Normalized) -> bool {
    use Sport::*;
    [Kiteboarding, WingFoiling, Parawinging].contains(&normalized.sport)
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

async fn insert_gear(pool: &PgPool, sesh_id: i32, gear_ids: &[i32]) -> Result<(), sqlx::Error> {
    let mut qb = QueryBuilder::new("insert into sesh_gear (sesh_id, gear_id)");

    qb.push_values(gear_ids, |mut b, gear_id| {
        b.push_bind(sesh_id).push_bind(gear_id);
    });
    qb.push(" returning id");

    let query = qb.build();
    let results = query.fetch_all(pool).await?;
    let ids: Vec<i32> = results
        .into_iter()
        .map(|row| row.get::<i32, _>("id"))
        .collect();

    println!("-- Inserted sesh gear records: {:?}", ids);
    Ok(())
}

async fn do_inserts(pool: &PgPool, normalized: Normalized) -> Result<(), sqlx::Error> {
    let sesh_id = insert_sesh(pool, &normalized).await?;
    println!("Inserted sesh {sesh_id}.");

    if is_wind_sport(&normalized) {
        let wind_sesh_id = insert_wind_sesh_details(pool, sesh_id, &normalized).await?;
        println!("-- Inserted wind sesh {wind_sesh_id}.")
    }

    let gear_ids = get_gear_ids(&normalized);
    if !gear_ids.is_empty() {
        insert_gear(pool, sesh_id, &gear_ids).await?;
    }

    Ok(())
}

pub async fn insert_records(normalized: Vec<Normalized>) -> Result<(), sqlx::Error> {
    let pool = PgPoolOptions::new()
        .max_connections(1)
        .connect("postgres://dylan:dylan@localhost:5432/seshtracker_dev")
        .await?;

    for n in normalized {
        do_inserts(&pool, n).await?;
    }

    Ok(())
}
