import { insertData } from "./db";
import { parseFile } from "./parsers";

function range(start: number, end: number) {
  const xs: number[] = [];
  for (let i = start; i <= end; i++) {
    xs.push(i);
  }
  return xs;
}

const filePath = (year: number) =>
  `/home/dylan/Desktop/Sesh Import/${year}.csv`;

const years = range(2012, 2026);

const main = async () => {
  const parsed = await Promise.all(
    years.map((year) => parseFile(filePath(year), year)),
  );
  const csvData = parsed.flat();
  await insertData(csvData);
};

main();
