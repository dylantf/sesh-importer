module Db

open System
open Dapper.FSharp.PostgreSQL

open Parsers
open Gear

// Register option type mapping for null values
OptionTypes.register ()

let private connect () =
    let conn =
        new Npgsql.NpgsqlConnection "Host=localhost;Port=5432;Database=seshtracker_dev;User Id=dylan;Password=dylan;"

    conn.Open()
    conn

[<CLIMutable>]
type private Sesh =
    { id: int
      user_id: int
      date: DateTime
      sport: string
      duration_seconds: int
      location_name: string option
      comments: string option
      created_at: DateTime
      updated_at: DateTime }

[<CLIMutable>]
type private WindSportSesh =
    { id: int
      sesh_id: int
      wind_avg: int
      wind_gust: int
      sesh_type: string }

[<CLIMutable>]
type private SeshGear = { id: int; sesh_id: int; gear_id: int }

type private MappedSesh =
    { Sesh: Sesh
      WindDetails: WindSportSesh option
      GearIds: int list }

let private seshTable = table'<Sesh> "seshes"
let private windSeshDetailsTable = table'<WindSportSesh> "wind_sesh_details"
let private seshGearTable = table'<SeshGear> "sesh_gear"

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
    let sesh: Sesh =
        { id = -1
          user_id = 1 // Dylan
          date = n.Date
          sport = stringifySport n.Sport
          duration_seconds = n.Hours * 60.0 * 60.0 |> Math.Round |> int
          location_name = n.Location
          comments = n.Comments
          created_at = DateTime.Now
          updated_at = DateTime.Now }

    let windDetails =
        if isWindSport n.Sport then
            match n with
            | { WindAvg = Some windAvg
                WindGust = Some windGust
                SeshType = Some seshType } ->
                Some
                    { id = -1
                      sesh_id = -1 // filled in later
                      wind_avg = windAvg
                      wind_gust = windGust
                      sesh_type = stringifySeshType seshType }
            | _ -> failwith $"Wind sport sesh missing data: {n}"
        else
            None

    { Sesh = sesh
      WindDetails = windDetails
      GearIds = seshGearIds n }

let private insertSeshes (conn: Npgsql.NpgsqlConnection) (mapped: MappedSesh list) : MappedSesh list =
    let seshRecords = mapped |> List.map (fun m -> m.Sesh)

    let insertedIds =
        insert {
            for s in seshTable do
                values seshRecords
                excludeColumn s.id
                excludeColumn s.created_at
                excludeColumn s.updated_at
        }
        |> conn.InsertOutputAsync<Sesh, {| id: int |}>
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.toList

    printfn $"Inserted {List.length insertedIds} seshes"

    List.map2
        (fun m (inserted: {| id: int |}) ->
            { m with
                Sesh = { m.Sesh with id = inserted.id } })
        mapped
        insertedIds

let private insertWindDetails (conn: Npgsql.NpgsqlConnection) (mapped: MappedSesh list) =
    let windRecords =
        mapped
        |> List.choose (fun m -> m.WindDetails |> Option.map (fun wd -> { wd with sesh_id = m.Sesh.id }))

    if not (List.isEmpty windRecords) then
        insert {
            for w in windSeshDetailsTable do
                values windRecords
                excludeColumn w.id
        }
        |> conn.InsertAsync
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> ignore

        printfn $"Inserted {List.length windRecords} wind details"

    mapped

let private insertGear (conn: Npgsql.NpgsqlConnection) (mapped: MappedSesh list) =
    let gearRecords: SeshGear list =
        mapped
        |> List.collect (fun m ->
            m.GearIds
            |> List.map (fun gearId ->
                { id = -1
                  sesh_id = m.Sesh.id
                  gear_id = gearId }))

    if not (List.isEmpty gearRecords) then
        insert {
            for sg in seshGearTable do
                values gearRecords
                excludeColumn sg.id
        }
        |> conn.InsertAsync
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> ignore

        printfn $"Inserted {List.length gearRecords} gear records"

    mapped

let insertAll (rows: Normalized list) =
    use conn = connect ()

    rows
    |> List.map mapNormalized
    |> insertSeshes conn
    |> insertWindDetails conn
    |> insertGear conn
    |> ignore
