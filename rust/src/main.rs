mod parsers;

fn import_path(year: &u32) -> String {
    format!("/home/dylan/Desktop/Sesh Import/{}.csv", year)
}

fn main() {
    let years = (2012..=2025).collect::<Vec<u32>>();

    let normalized = years
        .iter()
        .flat_map(|year| parsers::parse_file(year, &import_path(year)))
        .collect::<Vec<parsers::Normalized>>();

    println!("{:?}", normalized);
}
