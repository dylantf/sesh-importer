open Types

let maybe_string s =
  match String.trim s with
  | "" -> None
  | s -> Some s
;;

let parse_int s = maybe_string s |> Option.map int_of_string

let parse_many s =
  let split s = String.split_on_char ',' s in
  let items =
    maybe_string s |> Option.map (fun s -> split s |> List.map String.trim) |> Option.map (List.filter_map maybe_string)
  in
  match items with
  | None -> None
  | Some [] -> None
  | Some xs -> Some xs
;;

let normalize_sport sport =
  match String.trim sport with
  | "Kiteboarding" -> Kiteboarding
  | "SUP" -> SUP
  | "Skiing" -> Skiing
  | "Snowboarding" -> Snowboarding
  | "Mountain Biking" -> MountainBiking
  | "Hiking" -> Hiking
  | "Running" -> Running
  | "Paragliding" -> Paragliding
  | "Surfing" | "Surf" -> Surfing
  | "Wing foiling" -> WingFoiling
  | "Parawinging" -> Parawinging
  | other -> failwith (Printf.sprintf "Unhandled sport: `%s`" other)
;;

let normalize_sesh_type = function
  | Some "Spot" -> Some Spot
  | Some "Downwinder" -> Some Downwinder
  | Some "Roundwinder" -> Some Roundwinder
  | _ -> None
;;

let normalize_board_type = function
  | "Twintip" | "Twintp" -> Twintip (* Fix typo *)
  | "Hydrofoil" -> Hydrofoil
  | "Surfboard" | "Strapless" -> Surfboard
  | "SUP" -> SUPBoard
  | "Skis" -> Skis
  | "Snowboard" -> Snowboard
  | "Skim" -> Other
  | other -> failwith (Printf.sprintf "Unhandled board type: `%s`" other)
;;

let parse_board_types s = parse_many s |> Option.map (List.map normalize_board_type)

(* Some kite sizes are defined as "12m" and others just "12" *)
let normalize_kite_size kites =
  let digits_only s = Str.global_replace (Str.regexp "[^0-9]") "" s in
  kites |> Option.map (List.map digits_only)
;;

(* Parse date from M/D/YYYY to YYYY-MM-DD *)
let parse_date s = Scanf.sscanf s "%d/%d/%d" (fun m d y -> Printf.sprintf "%04d-%02d-%02d" y m d)

let parse_2012 row =
  match row with
  | [ date; sport; hours; lull; gust; kite_size; sesh_type; comments ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int lull
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite_size |> normalize_kite_size
    ; wing_size = None
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = None
    ; foil = None
    ; board = None
    ; location = None
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2012: unexpected row format"
;;

let parse_2013 row =
  match row with
  | [ date; sport; hours; lull; gust; kite; sesh_type; comments ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int lull
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = None
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = None
    ; foil = None
    ; board = None
    ; location = None
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2013: unexpected row format"
;;

let parse_2015 row =
  match row with
  (* Some rows have an empty trailing column *)
  | [ date; sport; hours; lull; gust; kite; sesh_type; location; comments ]
  | [ date; sport; hours; lull; gust; kite; sesh_type; location; comments; _ ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int lull
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = None
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = None
    ; foil = None
    ; board = None
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2015: unexpected row format"
;;

let parse_2014 row =
  match row with
  | [ date; sport; hours; lull; gust; kite_size; sesh_type; location; comments ]
  | [ date; sport; hours; lull; gust; kite_size; sesh_type; location; comments; _ ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int lull
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite_size |> normalize_kite_size
    ; wing_size = None
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = None
    ; foil = None
    ; board = None
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2014: unexpected row format"
;;

let parse_2016 row =
  match row with
  | date :: sport :: hours :: lull :: gust :: kite :: sesh_type :: board :: location :: comments :: _ ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int lull
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = None
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = parse_board_types board
    ; foil = None
    ; board = None
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2016: unexpected row format"
;;

let parse_2022 row =
  match row with
  | [ _; date; sport; hours; avg; gust; kite; wing; sesh_type; board_type; foil; foil_board; location; comments ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int avg
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = parse_many wing
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = parse_board_types board_type
    ; foil = parse_many foil
    ; board = maybe_string foil_board
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2022: unexpected row format"
;;

let parse_2024 row =
  match row with
  | [ _; date; sport; hours; avg; gust; kite; wing; board_type; foil; foil_board; location; sesh_type; comments ]
  | [ _; date; sport; hours; avg; gust; kite; wing; board_type; foil; foil_board; location; sesh_type; comments; _ ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int avg
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = parse_many wing
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = parse_board_types board_type
    ; foil = parse_many foil
    ; board = maybe_string foil_board
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2024: unexpected row format"
;;

let parse_2025 row =
  match row with
  | [ _; date; sport; hours; avg; gust; kite; wing; board_type; foil; board; location; sesh_type; comments ]
  | [ _; date; sport; hours; avg; gust; kite; wing; board_type; foil; board; location; sesh_type; comments; _ ] ->
    { date = parse_date date
    ; sport = normalize_sport sport
    ; hours = float_of_string hours
    ; wind_avg = parse_int avg
    ; wind_gust = parse_int gust
    ; kite_size = parse_many kite |> normalize_kite_size
    ; wing_size = parse_many wing
    ; sesh_type = maybe_string sesh_type |> normalize_sesh_type
    ; board_type = parse_board_types board_type
    ; foil = parse_many foil
    ; board = maybe_string board
    ; location = maybe_string location
    ; comments = maybe_string comments
    }
  | _ -> failwith "parse_2025: unexpected row format"
;;

let parse_file ~schema rows =
  let parser =
    match schema with
    | 2012 -> parse_2012
    | 2013 -> parse_2013
    | 2015 -> parse_2015
    | 2014 -> parse_2014
    | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 -> parse_2016
    | 2022 -> parse_2022
    | 2023 -> parse_2024
    | 2024 -> parse_2024
    | 2025 | 2026 -> parse_2025
    | other -> failwith (Printf.sprintf "Schema for `%d` not defined yet!" other)
  in
  List.map parser rows
;;
