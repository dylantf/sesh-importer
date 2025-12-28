module Db

open System
open Dapper.FSharp.PostgreSQL

open Parsers
open Gear
open System.Data

// Register option type mapping for null values
OptionTypes.register ()

let connect () =
    new Npgsql.NpgsqlConnection "Host=localhost;Port=5432;Database=seshtracker_dev;User Id=dylan;Password=dylan;"

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

type private SeshGear = { id: int; sesh_id: int; gear_id: int }

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

let private insertSesh (conn: IDbConnection) (n: Normalized) =
    let sesh: Sesh =
        { id = -1
          user_id = 1
          date = n.Date
          sport = stringifySport n.Sport
          duration_seconds = n.Hours * 60.0 * 60.0 |> Math.Round |> int
          location_name = n.Location
          comments = n.Comments
          created_at = DateTime.Now
          updated_at = DateTime.Now }

    let sesh =
        insert {
            for s in seshTable do
                value sesh
                excludeColumn s.id
                excludeColumn s.created_at
                excludeColumn s.updated_at
        }
        |> conn.InsertOutputAsync<Sesh, Sesh>
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.toList
        |> List.head

    printfn $"Inserted sesh: {sesh.id} {sesh.date} {sesh.sport}"
    sesh

let private insertWindSeshDetails (conn: IDbConnection) normalized seshId =
    let details =
        match normalized with
        | { WindAvg = Some windAvg
            WindGust = Some windGust
            SeshType = Some seshType } ->
            { id = -1
              sesh_id = seshId
              wind_avg = windAvg
              wind_gust = windGust
              sesh_type = stringifySeshType seshType }
        | _ -> failwith $"Kiteboarding sesh missing data: {normalized}"

    let inserted =
        insert {
            for t in windSeshDetailsTable do
                value details
                excludeColumn t.id
        }
        |> conn.InsertOutputAsync<WindSportSesh, WindSportSesh>
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.toList
        |> List.head

    printfn $"-- Inserted kiteboarding sesh {inserted.id}"

let isWindSport normalized =
    List.contains normalized.Sport [ Kiteboarding; WingFoiling; Parawinging ]

let private insertDetails (conn: IDbConnection) (n: Normalized) (seshId: int) =
    if isWindSport n then
        insertWindSeshDetails conn n seshId


let private seshGearIds (n: Normalized) =
    [ kiteIds; wingIds; boardIds; foilIds ] |> List.collect ((|>) n)

let private insertGear (conn: IDbConnection) (n: Normalized) (seshId: int) =
    match seshGearIds n with
    | [] -> ()
    | ids ->
        let records =
            ids
            |> List.map (fun gearId ->
                { id = -1
                  sesh_id = seshId
                  gear_id = gearId })

        printfn $"-- Inserting gear: {records}"

        insert {
            for sg in seshGearTable do
                values records
                excludeColumn sg.id
        }
        |> conn.InsertAsync
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> ignore

let insertSeshData (conn: IDbConnection) n =
    let sesh = insertSesh conn n
    insertDetails conn n sesh.id
    insertGear conn n sesh.id

let insertAll (rows: Normalized list) =
    let conn = connect ()
    List.iter (insertSeshData conn) rows
    conn.Close()
