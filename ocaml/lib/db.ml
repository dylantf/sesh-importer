open Types

let connection_uri =
  Uri.of_string "postgresql://dylan:dylan@localhost:5432/seshtracker_dev"
;;

let stringify_sport = function
  | Kiteboarding -> "kiteboarding"
  | SUP -> "sup"
  | Skiing -> "skiing"
  | Snowboarding -> "snowboarding"
  | MountainBiking -> "mountain biking"
  | Hiking -> "hiking"
  | Running -> "running"
  | Paragliding -> "paragliding"
  | Surfing -> "surfing"
  | WingFoiling -> "wing foiling"
  | Parawinging -> "parawinging"
;;

let stringify_sesh_type = function
  | Spot -> "spot"
  | Downwinder -> "downwinder"
  | Roundwinder -> "roundwinder"
;;

let is_wind_sport n =
  List.mem n.sport [ Kiteboarding; WingFoiling; Parawinging ]
;;

let sesh_gear_ids n =
  [ Gear.kite_ids; Gear.wing_ids; Gear.board_ids; Gear.foil_ids ]
  |> List.concat_map (fun f -> f n)
;;

(* SQL queries *)
let insert_sesh_query =
  Caqti_request.Infix.(
    Caqti_type.(t4 string string int (t2 (option string) (option string)))
    ->! Caqti_type.int)
    "INSERT INTO seshes (user_id, date, sport, duration_seconds, \
     location_name, comments) VALUES (1, $1::date, $2, $3, $4, $5) RETURNING \
     id"
;;

let insert_wind_details_query =
  Caqti_request.Infix.(Caqti_type.(t4 int int int string) ->. Caqti_type.unit)
    "INSERT INTO wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type) \
     VALUES ($1, $2, $3, $4)"
;;

let insert_gear_query =
  Caqti_request.Infix.(Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO sesh_gear (sesh_id, gear_id) VALUES ($1, $2)"
;;

let or_fail = function
  | Ok x -> x
  | Error err -> failwith (Caqti_error.show err)
;;

let insert_sesh (module Db : Caqti_lwt.CONNECTION) (n : normalized) =
  let duration_seconds = Float.to_int (Float.round (n.hours *. 3600.0)) in
  Db.find
    insert_sesh_query
    (n.date, stringify_sport n.sport, duration_seconds, (n.location, n.comments))
  |> Lwt_main.run
  |> or_fail
;;

let insert_wind_details (module Db : Caqti_lwt.CONNECTION) n sesh_id =
  match n.wind_avg, n.wind_gust, n.sesh_type with
  | Some avg, Some gust, Some st ->
    Db.exec
      insert_wind_details_query
      (sesh_id, avg, gust, stringify_sesh_type st)
    |> Lwt_main.run
    |> or_fail
  | _ -> failwith (Printf.sprintf "Wind sesh missing data: %s" n.date)
;;

let insert_details db n sesh_id =
  if is_wind_sport n then insert_wind_details db n sesh_id
;;

let insert_gear (module Db : Caqti_lwt.CONNECTION) n sesh_id =
  let ids = sesh_gear_ids n in
  if ids <> []
  then (
    Printf.printf
      "-- Inserting gear: [%s]\n"
      (ids |> List.map string_of_int |> String.concat ", ");
    ids
    |> List.iter (fun gear_id ->
      Db.exec insert_gear_query (sesh_id, gear_id) |> Lwt_main.run |> or_fail))
;;

let with_connection f =
  match Caqti_lwt_unix.connect connection_uri |> Lwt_main.run with
  | Ok db ->
    let result = f db in
    let (module Db : Caqti_lwt.CONNECTION) = db in
    Db.disconnect () |> Lwt_main.run;
    result
  | Error err -> failwith (Caqti_error.show err)
;;

let insert_sesh_data db n =
  let sesh_id = insert_sesh db n in
  Printf.printf
    "Inserted sesh: %d %s %s\n%!"
    sesh_id
    n.date
    (stringify_sport n.sport);
  insert_details db n sesh_id;
  insert_gear db n sesh_id
;;

let insert_all records =
  with_connection (fun db ->
    let (module Db : Caqti_lwt.CONNECTION) = db in
    Db.start () |> Lwt_main.run |> or_fail;
    try
      List.iter (insert_sesh_data db) records;
      Db.commit () |> Lwt_main.run |> or_fail
    with
    | e ->
      Db.rollback () |> Lwt_main.run |> or_fail;
      raise e)
;;
