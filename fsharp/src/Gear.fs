module Gear

open System
open Parsers

let private before date seshDate =
    let date = date |> DateTime.Parse |> _.AddDays(1)
    seshDate < date

let private after date seshDate =
    let date = DateTime.Parse date
    seshDate >= date

let private between (startDate, endDate) seshDate =
    let startDate = startDate |> DateTime.Parse
    let endDate = endDate |> DateTime.Parse |> _.AddDays(1)
    seshDate >= startDate && seshDate < endDate

let private deriveKiteId d size =
    match size with
    | "12" when before "2013-01-15" d -> Some 1
    | "8" when before "2013-01-15" d -> Some 2
    | "12" when between ("2013-01-16", "2013-12-07") d -> Some 3
    | "9" when between ("2013-01-16", "2013-12-07") d -> Some 4
    | "13" when between ("2013-12-08", "2014-12-31") d -> Some 6
    | "9" when between ("2013-12-08", "2014-12-31") d -> Some 7
    | "7" when between ("2013-12-08", "2014-12-31") d -> Some 8
    | "12" when between ("2015-01-01", "2017-02-05") d -> Some 9
    | "10" when between ("2016-10-09", "2018-01-24") d -> Some 10
    | "7" when between ("2016-10-09", "2019-10-04") d -> Some 11
    | "12" when between ("2017-02-06", "2022-08-10") d -> Some 12
    | "10" when between ("2018-01-25", "2020-12-31") d -> Some 13
    | "7" when between ("2019-10-05", "2020-12-31") d -> Some 14
    | "9" when after "2021-01-01" d -> Some 15
    | "7" when after "2021-01-01" d -> Some 16
    | _ -> None

let kiteIds sesh =
    match sesh.KiteSize with
    | None -> []
    | Some sizes -> sizes |> List.map (fun sz -> deriveKiteId sesh.Date sz) |> List.choose id

let private deriveFoilId d name =
    match name with
    | None when before "2019-05-26" d -> Some 17
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

let private isFoilSesh sesh =
    match sesh with
    | { BoardType = Some bt } -> List.contains Hydrofoil bt
    | _ -> false

let foilIds sesh =
    match isFoilSesh sesh, sesh.Foil with
    | true, None -> [ deriveFoilId sesh.Date None ]
    | true, Some foils -> foils |> List.map Some |> List.map (deriveFoilId sesh.Date)
    | _ -> []
    |> List.choose id

let private deriveBoardId d boardName =
    match boardName with
    | Some "Groove Skate"
    | None when after "2022-08-10" d -> Some 31
    | Some "Rocket v2 85L" -> Some 32
    | Some "Rocket v2 60L"
    | Some "Rocket 60L" -> Some 34
    | Some "Flying Fish 40L" -> Some 33
    | Some "LF Galaxy" -> Some 30
    | None when between ("2017-06-24", "2022-08-09") d -> Some 30
    | None when before "2017-06-23" d -> Some 17
    | _ -> None

// I only care about foilboards, throw away everything else
let boardIds sesh =
    match isFoilSesh sesh, sesh.Board with
    | false, _ -> []
    | true, None -> [ deriveBoardId sesh.Date None ]
    | true, Some boards -> boards |> List.map Some |> List.map (deriveBoardId sesh.Date)
    |> List.choose id

let private deriveWingId sport d size =
    match sport, size with
    | WingFoiling, "6m" -> Some 35
    | WingFoiling, "5m" when before "2024-01-01" d -> Some 36
    | WingFoiling, "5m" when after "2024-01-01" d -> Some 39
    | WingFoiling, "4m" when before "2024-01-01" d -> Some 37
    | WingFoiling, "4m" when after "2024-01-01" d -> Some 40
    | WingFoiling, "5.5m" -> Some 38
    | WingFoiling, "3m" -> Some 41
    | Parawinging, "4m" -> Some 42 // We'll just throw the parawing in here
    | _ -> None

let wingIds sesh =
    match sesh.Sport, sesh.WingSize with
    | WingFoiling, Some sizes
    | Parawinging, Some sizes -> sizes |> List.map (deriveWingId sesh.Sport sesh.Date) |> List.choose id
    | _, _ -> []
