open Types

let before end_date sesh_date = sesh_date < end_date
let after start_date sesh_date = sesh_date >= start_date

let between start_date end_date sesh_date =
  sesh_date >= start_date && sesh_date < end_date
;;

let derive_kite_id d size =
  match size with
  | "12" when before "2013-01-16" d -> Some 1
  | "8" when before "2013-01-16" d -> Some 2
  | "12" when between "2013-01-16" "2013-12-08" d -> Some 3
  | "9" when between "2013-01-16" "2013-12-08" d -> Some 4
  | "13" when between "2013-12-08" "2015-01-01" d -> Some 6
  | "9" when between "2013-12-08" "2015-01-01" d -> Some 7
  | "7" when between "2013-12-08" "2015-01-01" d -> Some 8
  | "12" when between "2015-01-01" "2017-02-06" d -> Some 9
  | "10" when between "2016-10-09" "2018-01-25" d -> Some 10
  | "7" when between "2016-10-09" "2019-10-05" d -> Some 11
  | "12" when between "2017-02-06" "2022-08-11" d -> Some 12
  | "10" when between "2018-01-25" "2021-01-01" d -> Some 13
  | "7" when between "2019-10-05" "2021-01-01" d -> Some 14
  | "9" when after "2021-01-01" d -> Some 15
  | "7" when after "2021-01-01" d -> Some 16
  | _ -> None
;;

let kite_ids sesh =
  match sesh.kite_size with
  | None -> []
  | Some sizes -> sizes |> List.filter_map (derive_kite_id sesh.date)
;;

let derive_board_id date board_name board_type =
  match board_type with
  | Hydrofoil ->
    (match board_name with
     | Some "Groove Skate" -> Some 31
     | None when after "2022-08-10" date -> Some 31
     | Some "Rocket v2 85L" -> Some 32
     | Some "Rocket v2 60L" | Some "Rocket 60L" -> Some 34
     | Some "Flying Fish 40L" -> Some 33
     | Some "LF Galaxy" -> Some 30
     | None when between "2017-06-24" "2022-08-10" date -> Some 30
     | None when before "2017-06-24" date -> Some 17
     | _ -> None)
  | Surfboard -> Some 43
  | Twintip -> Some 44
  | Skis -> Some 45
  | Snowboard -> Some 46
  | SUPBoard | Other -> None
;;

let is_foil_sesh sesh =
  match sesh.board_type with
  | Some bt -> List.mem Hydrofoil bt
  | None -> false
;;

let board_ids sesh =
  match sesh.board_type with
  | None -> []
  | Some board_types ->
    board_types |> List.filter_map (derive_board_id sesh.date sesh.board)
;;

let derive_foil_id d name =
  match name with
  | None when before "2019-05-27" d -> Some 17
  | None when after "2019-05-27" d -> Some 18
  | Some "Thruster" -> Some 18
  | Some "ART 999" -> Some 19
  | Some "ART 799" -> Some 20
  | Some "Phantom 1480" -> Some 21
  | Some "Seven Seas 1200" -> Some 22
  | Some "Phantom-S 840" -> Some 23
  | Some "Eagle 890" -> Some 24
  | Some "Eagle 990" -> Some 25
  | Some "Ypra-S 785" -> Some 26
  | Some "Ypra-S 1000" -> Some 27
  | Some "Veloce 890" -> Some 28
  | _ -> None
;;

let foil_ids sesh =
  match is_foil_sesh sesh, sesh.foil with
  | true, None -> [ derive_foil_id sesh.date None ] |> List.filter_map Fun.id
  | true, Some foils ->
    foils |> List.filter_map (fun f -> derive_foil_id sesh.date (Some f))
  | _, _ -> []
;;

let derive_wing_id sport d size =
  match sport, size with
  | WingFoiling, "6m" -> Some 35
  | WingFoiling, "5m" when before "2024-01-01" d -> Some 36
  | WingFoiling, "5m" when after "2024-01-01" d -> Some 39
  | WingFoiling, "4m" when before "2024-01-01" d -> Some 37
  | WingFoiling, "4m" when after "2024-01-01" d -> Some 40
  | WingFoiling, "5.5m" -> Some 38
  | WingFoiling, "3m" -> Some 41
  | Parawinging, "4m" -> Some 42 (* Parawing goes here *)
  | _, _ -> None
;;

let wing_ids sesh =
  match sesh.sport, sesh.wing_size with
  | WingFoiling, Some sizes | Parawinging, Some sizes ->
    sizes |> List.filter_map (derive_wing_id sesh.sport sesh.date)
  | _, _ -> []
;;
