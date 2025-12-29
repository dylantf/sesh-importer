import { pgTable, serial, integer, date, varchar, text } from "drizzle-orm/pg-core";

export const seshes = pgTable("seshes", {
  id: serial("id").primaryKey(),
  userId: integer("user_id").notNull(),
  date: date("date").notNull(),
  sport: varchar("sport", { length: 255 }).notNull(),
  durationSeconds: integer("duration_seconds").notNull(),
  locationName: varchar("location_name", { length: 255 }),
  comments: text("comments").notNull(),
});

export const windSeshDetails = pgTable("wind_sesh_details", {
  id: serial("id").primaryKey(),
  seshId: integer("sesh_id").notNull(),
  windAvg: integer("wind_avg").notNull(),
  windGust: integer("wind_gust").notNull(),
  seshType: varchar("sesh_type", { length: 255 }).notNull(),
});

export const seshGear = pgTable("sesh_gear", {
  id: serial("id").primaryKey(),
  seshId: integer("sesh_id").notNull(),
  gearId: integer("gear_id").notNull(),
});
