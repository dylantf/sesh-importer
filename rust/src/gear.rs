use crate::parsers::{BoardType, Normalized, Sport};
use chrono::NaiveDate;

fn parse_date(s: &str) -> NaiveDate {
    NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap()
}

fn before(date_str: &str, sesh_date: NaiveDate) -> bool {
    sesh_date <= parse_date(date_str)
}

fn after(date_str: &str, sesh_date: NaiveDate) -> bool {
    sesh_date >= parse_date(date_str)
}

fn between(start: &str, end: &str, sesh_date: NaiveDate) -> bool {
    after(start, sesh_date) && before(end, sesh_date)
}

fn derive_kite_id(sesh_date: NaiveDate, kite_size: &str) -> Option<i32> {
    match kite_size {
        "12" if before("2013-01-15", sesh_date) => Some(1),
        "8" if before("2013-01-15", sesh_date) => Some(2),
        "12" if between("2013-01-16", "2013-12-07", sesh_date) => Some(3),
        "9" if between("2013-01-16", "2013-12-07", sesh_date) => Some(4),
        "13" if between("2013-12-08", "2014-12-31", sesh_date) => Some(6),
        "9" if between("2013-12-08", "2014-12-31", sesh_date) => Some(7),
        "7" if between("2013-12-08", "2014-12-31", sesh_date) => Some(8),
        "12" if between("2015-01-01", "2017-02-05", sesh_date) => Some(9),
        "10" if between("2016-10-09", "2018-01-24", sesh_date) => Some(10),
        "7" if between("2016-10-09", "2019-10-04", sesh_date) => Some(11),
        "12" if between("2017-02-06", "2022-08-10", sesh_date) => Some(12),
        "10" if between("2018-01-25", "2020-12-31", sesh_date) => Some(13),
        "7" if between("2019-10-05", "2020-12-31", sesh_date) => Some(14),
        "9" if after("2021-01-01", sesh_date) => Some(15),
        "7" if after("2021-01-01", sesh_date) => Some(16),
        _ => None,
    }
}

fn derive_foil_id(d: NaiveDate, foil_name: &Option<&str>) -> Option<i32> {
    match foil_name {
        None if before("2019-05-26", d) => Some(17),
        None if after("2019-05-27", d) => Some(18),
        Some("Thruster") => Some(18),
        Some("ART 999") => Some(19),
        Some("ART 799") => Some(20),
        Some("Phantom 1480") => Some(21),
        Some("Seven Seas 1200") => Some(22),
        Some("Phantom-S 840") => Some(23),
        Some("Eagle 890") => Some(24),
        Some("Eagle 990") => Some(25),
        Some("Ypra-S 785") => Some(26),
        Some("Ypra-S 1000") => Some(27),
        Some("Veloce 890") => Some(28),
        _ => None,
    }
}

pub fn kite_ids(sesh: &Normalized) -> Vec<i32> {
    match &sesh.kite_size {
        None => vec![],
        Some(kites) => kites
            .iter()
            .filter_map(|size| derive_kite_id(sesh.date, size))
            .collect(),
    }
}

fn is_foil_sesh(sesh: &Normalized) -> bool {
    if let Normalized {
        board_type: Some(bt),
        ..
    } = sesh
    {
        bt.contains(&BoardType::Hydrofoil)
    } else {
        false
    }
}

pub fn foil_ids(sesh: &Normalized) -> Vec<i32> {
    let ids = match (is_foil_sesh(sesh), &sesh.foil) {
        (true, None) => vec![derive_foil_id(sesh.date, &None)],
        (true, Some(foil_names)) => foil_names
            .iter()
            .map(|name| derive_foil_id(sesh.date, &Some(name)))
            .collect(),
        _ => vec![],
    };

    ids.into_iter().flatten().collect()
}

fn derive_board_id(
    date: NaiveDate,
    board_type: &BoardType,
    board_name: Option<&str>,
) -> Option<i32> {
    use BoardType::*;
    match board_type {
        Hydrofoil => match board_name {
            Some("Groove Skate") | None if after("2022-08-10", date) => Some(31),
            Some("Rocket v2 85L") => Some(32),
            Some("Rocket v2 60L") | Some("Rocket 60L") => Some(34),
            Some("Flying Fish 40L") => Some(33),
            Some("LF Galaxy") => Some(30),
            None if between("2017-06-24", "2022-08-09", date) => Some(30),
            None if before("2017-06-23", date) => Some(17),
            _ => None,
        },
        Surfboard => Some(43),
        Twintip => Some(44),
        Skis => Some(45),
        Snowboard => Some(46),
        _ => None,
    }
}

pub fn board_ids(sesh: &Normalized) -> Vec<i32> {
    if let Some(board_types) = &sesh.board_type {
        board_types
            .iter()
            .filter_map(|bt| derive_board_id(sesh.date, bt, sesh.foil_board.as_deref()))
            .collect()
    } else {
        vec![]
    }
}

fn derive_wing_id(sport: &Sport, date: NaiveDate, size: &str) -> Option<i32> {
    use Sport::*;

    match (sport, size) {
        (WingFoiling, "6m") => Some(35),
        (WingFoiling, "5m") if before("2024-01-01", date) => Some(36),
        (WingFoiling, "5m") if after("2024-01-01", date) => Some(39),
        (WingFoiling, "4m") if before("2024-01-01", date) => Some(37),
        (WingFoiling, "4m") if after("2024-01-01", date) => Some(40),
        (WingFoiling, "5.5m") => Some(38),
        (WingFoiling, "3m") => Some(41),
        (Parawinging, "4m") => Some(42), // We'll just throw the parawing in here
        _ => None,
    }
}

pub fn wing_ids(sesh: &Normalized) -> Vec<i32> {
    match (&sesh.sport, &sesh.wing_size) {
        (Sport::WingFoiling, Some(sizes)) | (Sport::Parawinging, Some(sizes)) => sizes
            .iter()
            .filter_map(|sz| derive_wing_id(&sesh.sport, sesh.date, sz))
            .collect(),
        (_, _) => vec![],
    }
}
