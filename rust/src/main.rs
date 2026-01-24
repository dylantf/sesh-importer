use crate::database::insert_records;

mod database;
mod gear;
mod parsers;

fn import_path(year: &i32) -> String {
    let home = std::env::var("HOME").expect("HOME environment variable not set");
    std::path::PathBuf::from(home)
        .join("Desktop")
        .join("Sesh Import")
        .join(format!("{}.csv", year))
        .to_string_lossy()
        .to_string()
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    let years = (2012..=2026).collect::<Vec<i32>>();

    let normalized = years
        .iter()
        .flat_map(|year| parsers::parse_file(year, &import_path(year)))
        .collect::<Vec<parsers::Normalized>>();

    insert_records(normalized).await
}
