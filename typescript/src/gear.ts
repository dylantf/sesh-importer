import type { BoardType, Normalized, Sport } from "./parsers";

const parseDate = (dateStr: string): Date => {
  const [year, month, day] = dateStr.split("-").map(Number);
  return new Date(year, month - 1, day);
};

const before = (dateStr: string, d: Date) => d <= parseDate(dateStr);
const after = (dateStr: string, d: Date) => d >= parseDate(dateStr);
const between = (start: string, end: string, d: Date) =>
  d >= parseDate(start) && d <= parseDate(end);

function kiteId(d: Date, kiteSize: string): number | null {
  switch (kiteSize) {
    case "12":
      if (before("2013-01-15", d)) return 1;
      if (between("2013-01-16", "2013-12-07", d)) return 3;
      if (between("2015-01-01", "2017-02-05", d)) return 9;
      if (between("2017-02-06", "2022-08-10", d)) return 12;
      return null;
    case "8":
      if (before("2013-01-15", d)) return 2;
      return null;
    case "9":
      if (between("2013-01-16", "2013-12-07", d)) return 4;
      if (between("2013-12-08", "2014-12-31", d)) return 7;
      if (after("2021-01-01", d)) return 15;
      return null;
    case "13":
      if (between("2013-12-08", "2014-12-31", d)) return 6;
      return null;
    case "7":
      if (between("2013-12-08", "2014-12-31", d)) return 8;
      if (between("2016-10-09", "2019-10-04", d)) return 11;
      if (between("2019-10-05", "2020-12-31", d)) return 14;
      if (after("2021-01-01", d)) return 16;
      return null;
    case "10":
      if (between("2016-10-09", "2018-01-24", d)) return 10;
      if (between("2018-01-25", "2020-12-31", d)) return 13;
      return null;
    default:
      return null;
  }
}

export function kiteIds(row: Normalized): number[] {
  if (row.sport !== "kiteboarding" || !row.kiteSize) return [];
  return row.kiteSize.reduce<number[]>((acc, size) => {
    const id = kiteId(row.date, size);
    if (id) acc.push(id);
    return acc;
  }, []);
}

function hydrofoilId(d: Date, foilName: string | null): number | null {
  if (!foilName) {
    return before("2019-05-26", d) ? 17 : 18;
  }
  switch (foilName) {
    case "Thruster":
      return 18;
    case "ART 999":
      return 19;
    case "ART 799":
      return 20;
    case "Phantom 1480":
      return 21;
    case "Seven Seas 1200":
      return 22;
    case "Phantom-S 840":
      return 23;
    case "Eagle 890":
      return 24;
    case "Eagle 990":
      return 25;
    case "Ypra-S 785":
    case "Ypra S 785":
      return 26;
    case "Ypra-S 1000":
    case "Ypra S 1000":
      return 27;
    case "Veloce 890":
      return 28;
    default:
      return null;
  }
}

function isFoilSesh(row: Normalized): boolean {
  return row.boardType?.includes("hydrofoil") ?? false;
}

export function foilIds(row: Normalized): number[] {
  if (!isFoilSesh(row)) return [];
  if (!row.foil) {
    const id = hydrofoilId(row.date, null);
    return id ? [id] : [];
  }
  return row.foil.reduce<number[]>((acc, name) => {
    const id = hydrofoilId(row.date, name);
    if (id) acc.push(id);
    return acc;
  }, []);
}

function boardId(
  date: Date,
  boardName: string | null,
  bt: BoardType,
): number | null {
  if (bt === "hydrofoil") {
    switch (boardName) {
      case "Groove Skate":
        return 31;
      case "Rocket v2 85L":
        return 32;
      case "Rocket v2 60L":
      case "Rocket 60L":
        return 34;
      case "Flying Fish 40L":
        return 33;
      case "LF Galaxy":
        return 30;
      default:
        if (between("2017-06-24", "2022-08-09", date)) return 30;
        if (before("2017-06-23", date)) return 17;
        if (after("2022-08-10", date)) return 31;
        return null;
    }
  }
  switch (bt) {
    case "surfboard":
      return 43;
    case "twintip":
      return 44;
    case "skis":
      return 45;
    case "snowboard":
      return 46;
    default:
      return null;
  }
}

export function boardIds(row: Normalized): number[] {
  if (!row.boardType) return [];
  return row.boardType.reduce<number[]>((acc, bt) => {
    const id = boardId(row.date, row.board, bt);
    if (id) acc.push(id);
    return acc;
  }, []);
}

function wingId(sport: Sport, day: Date, size: string): number | null {
  switch (size) {
    case "6m":
      return 35;
    case "5m":
      return before("2024-01-01", day) ? 36 : 39;
    case "4m":
      if (sport === "parawinging") return 42;
      return before("2024-01-01", day) ? 37 : 40;
    case "5.5m":
      return 38;
    case "3m":
      return 41;
    default:
      return null;
  }
}

function usesWing(sport: Sport): boolean {
  return sport === "wing_foiling" || sport === "parawinging";
}

export function wingIds(row: Normalized): number[] {
  if (!usesWing(row.sport) || !row.wingSize) return [];
  return row.wingSize.reduce<number[]>((acc, size) => {
    const id = wingId(row.sport, row.date, size);
    if (id) acc.push(id);
    return acc;
  }, []);
}
