module Db

open System
open Npgsql.FSharp

open Parsers
open Gear

type private SeshRow =
    { UserId: int
      Date: DateTime
      Sport: string
      DurationSeconds: int
      LocationName: string option
      Comments: string option }

type private WindSportDetailsRow =
    { WindAvg: int
      WindGust: int
      SeshType: string }

type private MappedSesh =
    { Sesh: SeshRow
      WindSportDetails: WindSportDetailsRow option
      GearIds: int list }

type private InsertedMappedSesh = (int * MappedSesh)

let private stringifySport =
    function
    | Kiteboarding -> "kiteboarding"
    | Sport.SUP -> "sup"
    | Skiing -> "skiing"
    | Snowboarding -> "snowboarding"
    | MountainBiking -> "mountain biking"
    | Hiking -> "hiking"
    | Running -> "running"
    | Paragliding -> "paragliding"
    | Surfing -> "surfing"
    | WingFoiling -> "wing foiling"
    | Parawinging -> "parawinging"

let private stringifySeshType =
    function
    | Spot -> "spot"
    | Downwinder -> "downwinder"
    | Roundwinder -> "roundwinder"

let private isWindSport sport =
    List.contains sport [ Kiteboarding; WingFoiling; Parawinging ]

let private seshGearIds (n: Normalized) =
    [ kiteIds; wingIds; boardIds; foilIds ] |> List.collect ((|>) n)

// Map Normalized -> MappedSesh for batch insert
let private mapNormalized (n: Normalized) : MappedSesh =
    let duration = n.Hours * 60.0 * 60.0 |> Math.Round |> int

    let sesh =
        { UserId = 1 // Dylan
          Date = n.Date
          Sport = stringifySport n.Sport
          DurationSeconds = duration
          LocationName = n.Location
          Comments = n.Comments }

    let windDetails =
        if isWindSport n.Sport then
            match n with
            | { WindAvg = Some windAvg
                WindGust = Some windGust
                SeshType = Some seshType } ->
                Some
                    { WindAvg = windAvg
                      WindGust = windGust
                      SeshType = stringifySeshType seshType }
            | _ -> failwith $"Wind sport sesh missing data: {n}"
        else
            None

    { Sesh = sesh
      WindSportDetails = windDetails
      GearIds = seshGearIds n }

let private insertSeshes (conn: Npgsql.NpgsqlConnection) (mapped: MappedSesh list) : InsertedMappedSesh list =
    let seshes = List.map _.Sesh mapped

    let insertSeshesSql =
        """
        insert into seshes (user_id, date, sport, duration_seconds, location_name, comments)
        select * from unnest(@userIds::int[], @dates::date[], @sports::text[], @durations::int[], @locations::text[], @comments::text[])
        returning id
        """

    let ids =
        conn
        |> Sql.existingConnection
        |> Sql.query insertSeshesSql
        |> Sql.parameters
            [ "userIds", Array.replicate seshes.Length 1 |> Sql.intArray
              "dates",
              seshes
              |> List.map (_.Date.ToString("yyyy-MM-dd"))
              |> List.toArray
              |> Sql.stringArray
              "sports", seshes |> List.map _.Sport |> List.toArray |> Sql.stringArray
              "durations", seshes |> List.map _.DurationSeconds |> List.toArray |> Sql.intArray
              "locations",
              seshes
              |> List.map (_.LocationName >> Option.toObj)
              |> List.toArray
              |> Sql.stringArray
              "comments",
              seshes
              |> List.map (_.Comments >> Option.toObj)
              |> List.toArray
              |> Sql.stringArray ]
        |> Sql.execute (fun read -> read.int "id")

    if ids.Length <> seshes.Length then
        failwith $"Inserted a different number of seshes than expected! {ids.Length} vs {seshes.Length}"
    else
        printfn $"Inserted {ids.Length} seshes"

    List.map2 (fun id sesh -> id, sesh) ids mapped

let private insertWindDetails (conn: Npgsql.NpgsqlConnection) (mapped: InsertedMappedSesh list) =
    let windRecords =
        mapped
        |> List.choose (fun (seshId, m) -> m.WindSportDetails |> Option.map (fun wd -> seshId, wd))

    if not (List.isEmpty windRecords) then
        let sql =
            """
            insert into wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type)
            select * from unnest(@seshIds::int[], @windAvgs::int[], @windGusts::int[], @seshTypes::text[])
            """

        let rowsAffected =
            conn
            |> Sql.existingConnection
            |> Sql.query sql
            |> Sql.parameters
                [ "seshIds", windRecords |> List.map fst |> List.toArray |> Sql.intArray
                  "windAvgs", windRecords |> List.map (snd >> _.WindAvg) |> List.toArray |> Sql.intArray
                  "windGusts", windRecords |> List.map (snd >> _.WindGust) |> List.toArray |> Sql.intArray
                  "seshTypes", windRecords |> List.map (snd >> _.SeshType) |> List.toArray |> Sql.stringArray ]
            |> Sql.executeNonQuery

        printfn $"Inserted {rowsAffected} wind details"

    mapped

let private insertGear (conn: Npgsql.NpgsqlConnection) (mapped: InsertedMappedSesh list) =
    let gearRecords =
        mapped
        |> List.collect (fun (seshId, m) -> m.GearIds |> List.map (fun gearId -> seshId, gearId))

    if not (List.isEmpty gearRecords) then
        let sql =
            """
            insert into sesh_gear (sesh_id, gear_id)
            select * from unnest(@seshIds::int[], @gearIds::int[])
            """

        let rowsAffected =
            conn
            |> Sql.existingConnection
            |> Sql.query sql
            |> Sql.parameters
                [ "seshIds", gearRecords |> List.map fst |> List.toArray |> Sql.intArray
                  "gearIds", gearRecords |> List.map snd |> List.toArray |> Sql.intArray ]
            |> Sql.executeNonQuery

        printfn $"Inserted {rowsAffected} gear records"

    mapped

let insertAll (rows: Normalized list) =
    use conn =
        new Npgsql.NpgsqlConnection "Host=localhost;Port=5432;Database=seshtracker_dev;User Id=dylan;Password=dylan;"

    conn.Open()

    rows
    |> List.map mapNormalized
    |> insertSeshes conn
    |> insertWindDetails conn
    |> insertGear conn
    |> ignore

    conn.Close()
