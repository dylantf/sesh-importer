import { drizzle } from "drizzle-orm/postgres-js";
import postgres from "postgres";
import { kiteIds, foilIds, boardIds, wingIds } from "./gear";
import type { Normalized, Sport } from "./parsers";
import { seshes, windSeshDetails, seshGear } from "./schema";

const client = postgres(
  "postgres://dylan:dylan@localhost:5432/seshtracker_dev"
);
const db = drizzle(client);

const USER_ID = 1;

const sportToString = (sport: Sport): string => {
  const map: Record<Sport, string> = {
    kiteboarding: "kiteboarding",
    sup: "sup",
    skiing: "skiing",
    snowboarding: "snowboarding",
    mountain_biking: "mountain biking",
    hiking: "hiking",
    running: "running",
    paragliding: "paragliding",
    surfing: "surfing",
    wing_foiling: "wing foiling",
    parawinging: "parawinging",
  };
  return map[sport];
};

const isWindSport = (sport: Sport): boolean =>
  sport === "kiteboarding" ||
  sport === "wing_foiling" ||
  sport === "parawinging";

interface WindDetails {
  windAvg: number;
  windGust: number;
  seshType: string;
}

interface Prepped {
  userId: number;
  date: string;
  sport: string;
  durationSeconds: number;
  locationName: string | null;
  comments: string;
  windDetails: WindDetails | null;
  gearIds: number[];
}

const formatDate = (d: Date): string => d.toISOString().split("T")[0];

const prepWindDetails = (n: Normalized): WindDetails | null => {
  if (!isWindSport(n.sport)) return null;
  if (n.windAvg === null || n.windGust === null || n.seshType === null) {
    return null;
  }
  return {
    windAvg: n.windAvg,
    windGust: n.windGust,
    seshType: n.seshType,
  };
};

const prep = (n: Normalized): Prepped => ({
  userId: USER_ID,
  date: formatDate(n.date),
  sport: sportToString(n.sport),
  durationSeconds: Math.ceil(n.hours * 3600),
  locationName: n.location,
  comments: n.comments,
  windDetails: prepWindDetails(n),
  gearIds: [...kiteIds(n), ...foilIds(n), ...boardIds(n), ...wingIds(n)],
});

const insertSeshes = async (
  prepped: Prepped[]
): Promise<{ id: number; prepped: Prepped }[]> => {
  const rows = await db
    .insert(seshes)
    .values(prepped)
    .returning({ id: seshes.id });

  console.log(`Inserted ${rows.length} seshes`);

  return rows.map((row, i) => ({
    id: row.id,
    prepped: prepped[i],
  }));
};

const insertWindDetails = async (
  seshesWithIds: { id: number; prepped: Prepped }[]
): Promise<void> => {
  const toInsert = seshesWithIds.reduce<
    Array<WindDetails & { seshId: number }>
  >((acc, s) => {
    if (s.prepped.windDetails) {
      acc.push({ seshId: s.id, ...s.prepped.windDetails });
    }
    return acc;
  }, []);

  if (toInsert.length === 0) return;

  await db.insert(windSeshDetails).values(toInsert);
  console.log(`Inserted ${toInsert.length} wind details`);
};

const insertGear = async (
  seshesWithIds: { id: number; prepped: Prepped }[]
): Promise<void> => {
  const toInsert = seshesWithIds.flatMap((s) =>
    s.prepped.gearIds.map((gearId) => ({ seshId: s.id, gearId }))
  );
  if (toInsert.length === 0) return;

  await db.insert(seshGear).values(toInsert);

  console.log(`Inserted ${toInsert.length} gear records`);
};

export const insertData = async (normalized: Normalized[]): Promise<void> => {
  const prepped = normalized.map(prep);
  const seshesWithIds = await insertSeshes(prepped);
  await insertWindDetails(seshesWithIds);
  await insertGear(seshesWithIds);
  await client.end();
};
