import { parseFile } from "./parsers";
import { insertData } from "./db";

const filePath = (year: number) =>
  `/home/dylan/Desktop/Sesh Import/${year}.csv`;

const years = Array.from({ length: 2025 - 2012 + 1 }, (_, i) => 2012 + i);

const main = async () => {
  const parsed = await Promise.all(
    years.map((year) => parseFile(filePath(year), year))
  );
  const csvData = parsed.flat();
  await insertData(csvData);
};

main();
