import Papa from "papaparse";

// Types

export type Sport =
  | "kiteboarding"
  | "sup"
  | "skiing"
  | "snowboarding"
  | "mountain_biking"
  | "hiking"
  | "running"
  | "paragliding"
  | "surfing"
  | "wing_foiling"
  | "parawinging";

export type SeshType = "spot" | "downwinder" | "roundwinder";

export type BoardType =
  | "twintip"
  | "hydrofoil"
  | "surfboard"
  | "sup"
  | "skis"
  | "snowboard"
  | "other";

export interface Normalized {
  date: Date;
  sport: Sport;
  hours: number;
  windAvg: number | null;
  windGust: number | null;
  kiteSize: string[] | null;
  wingSize: string[] | null;
  seshType: SeshType | null;
  boardType: BoardType[] | null;
  foil: string[] | null;
  board: string | null;
  location: string | null;
  comments: string;
}

// Helpers

const maybeString = (str: string | undefined): string | null => {
  if (!str) return null;
  const trimmed = str.trim();
  return trimmed === "" ? null : trimmed;
};

const parseMany = (str: string | undefined): string[] | null => {
  const trimmed = maybeString(str);
  if (!trimmed) return null;
  const parts = trimmed.split(",").map((s) => s.trim());
  return parts.length > 0 ? parts : null;
};

const normalizeSport = (s: string | undefined): Sport => {
  if (!s) throw new Error("Sport is required");
  switch (s.trim()) {
    case "Kiteboarding":
      return "kiteboarding";
    case "SUP":
      return "sup";
    case "Skiing":
      return "skiing";
    case "Snowboarding":
      return "snowboarding";
    case "Mountain Biking":
      return "mountain_biking";
    case "Hiking":
      return "hiking";
    case "Running":
      return "running";
    case "Paragliding":
      return "paragliding";
    case "Surfing":
    case "Surf":
      return "surfing";
    case "Wing foiling":
      return "wing_foiling";
    case "Parawinging":
      return "parawinging";
    default:
      throw new Error(`Unhandled sport: \`${s}\``);
  }
};

const normalizeSeshType = (s: string | null | undefined): SeshType | null => {
  if (!s) return null;
  switch (s.trim()) {
    case "Spot":
      return "spot";
    case "Downwinder":
      return "downwinder";
    case "Roundwinder":
      return "roundwinder";
    default:
      throw new Error(`Unhandled session type: \`${s}\``);
  }
};

const normalizeBoardType = (bt: string): BoardType => {
  switch (bt.trim()) {
    case "Twintip":
    case "Twintp":
      return "twintip";
    case "Hydrofoil":
      return "hydrofoil";
    case "Surfboard":
    case "Strapless":
      return "surfboard";
    case "SUP":
      return "sup";
    case "Skis":
      return "skis";
    case "Snowboard":
      return "snowboard";
    case "Skim":
      return "other";
    default:
      throw new Error(`Unhandled board type: \`${bt}\``);
  }
};

const parseDate = (dateStr: string | undefined): Date => {
  if (!dateStr) throw new Error("Date is required");
  const [month, day, year] = dateStr.split("/").map(Number);
  if (!month || !day || !year) throw new Error(`Invalid date: ${dateStr}`);
  return new Date(year, month - 1, day);
};

const parseKiteSize = (str: string | undefined): string[] | null =>
  parseMany(str)?.map((s) => s.replace("m", "")) ?? null;

const parseBoardType = (bt: string | undefined): BoardType[] | null =>
  parseMany(bt)?.map(normalizeBoardType) ?? null;

const maybeInt = (val: string | undefined): number | null => {
  if (!val) return null;
  const n = parseInt(val, 10);
  return isNaN(n) ? null : n;
};

const parseFloat_ = (val: string | undefined): number => {
  if (!val) throw new Error("Float value is required");
  return parseFloat(val);
};

type CsvRow = Record<string, string>;

const parse2012 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Lull (kn)"]),
  windGust: maybeInt(row["Gust (kn)"]),
  kiteSize: parseKiteSize(row["Kite Size"]),
  wingSize: null,
  seshType: normalizeSeshType(row["Type"]),
  boardType: null,
  foil: null,
  board: null,
  location: null,
  comments: row["Comments"] ?? "",
});

const parse2013 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Lull"]),
  windGust: maybeInt(row["Gust"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: null,
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: null,
  foil: null,
  board: null,
  location: null,
  comments: row["Comments"] ?? "",
});

const parse2014 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Day"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Lull (kn)"]),
  windGust: maybeInt(row["Gust (kn)"]),
  kiteSize: parseKiteSize(row["Kite Size"]),
  wingSize: null,
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: null,
  foil: null,
  board: null,
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const parse2015 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Lull"]),
  windGust: maybeInt(row["Gust"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: null,
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: null,
  foil: null,
  board: null,
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const parse2016 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Lull (kts)"]),
  windGust: maybeInt(row["Gust (kts)"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: null,
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: parseBoardType(row["Board"]),
  foil: null,
  board: null,
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const parse2022 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Avg (kts)"]),
  windGust: maybeInt(row["Gust (kts)"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: parseMany(row["Wing"]),
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: parseBoardType(row["Board Type"]),
  foil: parseMany(row["Foil"]),
  board: maybeString(row["Foil Board"]),
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const parse2024 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Avg (kts)"]),
  windGust: maybeInt(row["Gust (kts)"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: parseMany(row["Wing"]),
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: parseBoardType(row["Board Type"]),
  foil: parseMany(row["Foil"]),
  board: maybeString(row["Foil Board"]),
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const parse2025 = (row: CsvRow): Normalized => ({
  date: parseDate(row["Date"]),
  sport: normalizeSport(row["Sport"]),
  hours: parseFloat_(row["Hours"]),
  windAvg: maybeInt(row["Avg (kts)"]),
  windGust: maybeInt(row["Gust (kts)"]),
  kiteSize: parseKiteSize(row["Kite"]),
  wingSize: parseMany(row["Wing"]),
  seshType: normalizeSeshType(maybeString(row["Type"])),
  boardType: parseBoardType(row["Board Type"]),
  foil: parseMany(row["Foil"]),
  board: maybeString(row["Board"]),
  location: maybeString(row["Location"]),
  comments: row["Comments"] ?? "",
});

const getParser = (year: number): ((row: CsvRow) => Normalized) => {
  switch (year) {
    case 2012:
      return parse2012;
    case 2013:
      return parse2013;
    case 2014:
      return parse2014;
    case 2015:
      return parse2015;
    case 2016:
    case 2017:
    case 2018:
    case 2019:
    case 2020:
    case 2021:
      return parse2016;
    case 2022:
    case 2023:
      return parse2022;
    case 2024:
      return parse2024;
    case 2025:
      return parse2025;
    default:
      throw new Error(`Parser not implemented for year: ${year}`);
  }
};

export const parseFile = async (
  path: string,
  year: number
): Promise<Normalized[]> => {
  const content = await Bun.file(path).text();
  const { data } = Papa.parse<CsvRow>(content, { header: true });
  const parser = getParser(year);
  return data.filter((row) => row["Date"]).map(parser);
};
