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
type private KiteboardingSesh =
    { id: int
      sesh_id: int
      wind_avg: int
      wind_gust: int
      sesh_type: string }

[<CLIMutable>]
type private WingFoilingSesh = KiteboardingSesh

type private SeshGear = { id: int; sesh_id: int; gear_id: int }

let private seshTable = table'<Sesh> "seshes"
let private kiteboardingSeshTable = table'<KiteboardingSesh> "kiteboarding_seshes"
let private wingFoilingSeshTable = table'<WingFoilingSesh> "wing_foiling_seshes"
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

let private insertKiteboardingSesh (conn: IDbConnection) normalized seshId =
    let kbSesh =
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
            for t in kiteboardingSeshTable do
                value kbSesh
                excludeColumn t.id
        }
        |> conn.InsertOutputAsync<KiteboardingSesh, KiteboardingSesh>
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.toList
        |> List.head

    printfn $"-- Inserted kiteboarding sesh {inserted.id}"


let private insertWingFoilingSesh (conn: IDbConnection) normalized seshId =
    let wfSesh =
        match normalized with
        | { WindAvg = Some windAvg
            WindGust = Some windGust
            SeshType = Some seshType } ->
            { id = -1
              sesh_id = seshId
              wind_avg = windAvg
              wind_gust = windGust
              sesh_type = stringifySeshType seshType }
        | _ -> failwith $"Wing foiling sesh missing data: {normalized}"

    let inserted =
        insert {
            for t in wingFoilingSeshTable do
                value wfSesh
                excludeColumn t.id
        }
        |> conn.InsertOutputAsync<WingFoilingSesh, WingFoilingSesh>
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.toList
        |> List.head

    printfn $"-- Inserted wing foiling sesh {inserted.id}"

let private insertDetails (conn: IDbConnection) (n: Normalized) (seshId: int) =
    match n with
    | { Sport = Kiteboarding } as kbSesh -> insertKiteboardingSesh conn kbSesh seshId
    | { Sport = WingFoiling } as wfSesh -> insertWingFoilingSesh conn wfSesh seshId
    | { Sport = Parawinging } as pwSesh -> insertWingFoilingSesh conn pwSesh seshId
    | _ -> ()

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

let insertSeshData n =
    let conn = connect ()
    let sesh = insertSesh conn n
    insertDetails conn n sesh.id
    insertGear conn n sesh.id
    conn.Close()
