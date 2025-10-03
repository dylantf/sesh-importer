use crate::database::insert_records;

mod database;
mod gear;
mod parsers;

fn import_path(year: &i32) -> String {
    format!("/home/dylan/Desktop/Sesh Import/{}.csv", year)
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let years = (2012..=2025).collect::<Vec<i32>>();

    let normalized = years
        .iter()
        .flat_map(|year| parsers::parse_file(year, &import_path(year)))
        .collect::<Vec<parsers::Normalized>>();

    insert_records(normalized).await
}
