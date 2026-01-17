module Parsers

open FSharp.Data
open System

type Sport =
    | Kiteboarding
    | SUP
    | Skiing
    | Snowboarding
    | MountainBiking
    | Hiking
    | Running
    | Paragliding
    | Surfing
    | WingFoiling
    | Parawinging

type SeshType =
    | Spot
    | Downwinder
    | Roundwinder

type BoardType =
    | Twintip
    | Hydrofoil
    | Surfboard
    | SUP
    | Skis
    | Snowboard
    | Other

type Normalized =
    { Date: DateTime
      Sport: Sport
      Hours: double
      WindAvg: int option
      WindGust: int option
      KiteSize: string list option
      WingSize: string list option
      SeshType: SeshType option
      BoardType: BoardType list option
      Foil: string list option
      Board: string option
      Location: string option
      Comments: string option }

let private maybeString (s: string) : string option =
    match s.Trim() with
    | "" -> None
    | s -> Some s

let private parseInt = maybeString >> Option.map int

let private parseMany (s: string) =
    let split (s: string) = s.Split "," |> Array.toList
    let trim (s: string) = s.Trim()

    let items =
        s
        |> maybeString
        |> Option.map (split >> List.map trim)
        |> Option.map (List.map maybeString >> List.choose id)

    match items with
    | None -> None
    | Some [] -> None
    | Some xs -> Some xs

let private normalizeSport (sport: string) =
    match sport.Trim() with
    | "Kiteboarding" -> Kiteboarding
    | "SUP" -> Sport.SUP
    | "Skiing" -> Skiing
    | "Snowboarding" -> Snowboarding
    | "Mountain Biking" -> MountainBiking
    | "Hiking" -> Hiking
    | "Running" -> Running
    | "Paragliding" -> Paragliding
    | "Surfing" -> Surfing
    | "Surf" -> Surfing
    | "Wing foiling" -> WingFoiling
    | "Parawinging" -> Parawinging
    | other -> failwith $"Unhandled sport: `{other}`"

let private normalizeSeshType =
    function
    | Some "Spot" -> Some Spot
    | Some "Downwinder" -> Some Downwinder
    | Some "Roundwinder" -> Some Roundwinder
    | _ -> None

let private normalizeBoardType =
    function
    | "Twintip"
    | "Twintp" -> Twintip // Fix typo
    | "Hydrofoil" -> Hydrofoil
    | "Surfboard"
    | "Strapless" -> Surfboard
    | "SUP" -> SUP
    | "Skis" -> Skis
    | "Snowboard" -> Snowboard
    | "Skim" -> Other
    | other -> failwith $"Unhandled board type: `{other}`"

let private parseBoardTypes s =
    s |> parseMany |> Option.map (List.map normalizeBoardType)

// Some kite sizes are defined as "12m" and others just "12"
let private normalizeKiteSize (kites: string list option) =
    kites |> Option.map (List.map (fun kite -> kite.Replace("m", "")))

let private parse2012 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kn)" |> parseInt
      WindGust = row.GetColumn "Gust (kn)" |> parseInt
      KiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = None
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2013 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull" |> parseInt
      WindGust = row.GetColumn "Gust" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = None
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2014 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Day" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kn)" |> parseInt
      WindGust = row.GetColumn "Gust (kn)" |> parseInt
      KiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2015 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull" |> parseInt
      WindGust = row.GetColumn "Gust" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2016 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = row.GetColumn "Board" |> parseBoardTypes
      Foil = None
      Board = None
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2022 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = row.GetColumn "Wing" |> parseMany
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = row.GetColumn "Board Type" |> parseBoardTypes
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Foil Board" |> maybeString
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2024 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = row.GetColumn "Wing" |> parseMany
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = row.GetColumn "Board Type" |> parseBoardTypes
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Foil Board" |> maybeString
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2025 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> normalizeSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = row.GetColumn "Wing" |> parseMany
      SeshType = row.GetColumn "Type" |> maybeString |> normalizeSeshType
      BoardType = row.GetColumn "Board Type" |> parseBoardTypes
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Board" |> maybeString
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let parseFile (schema: int) (path: string) =
    CsvFile.Load(path).Rows
    |> Seq.map (
        match schema with
        | 2012 -> parse2012
        | 2013 -> parse2013
        | 2014 -> parse2014
        | 2015 -> parse2015
        | 2016
        | 2017
        | 2018
        | 2019
        | 2020
        | 2021 -> parse2016
        | 2022
        | 2023 -> parse2022
        | 2024 -> parse2024
        | 2025
        | 2026 -> parse2025
        | other -> failwith $"Schema for `{other}` not defined yet!"
    )
    |> Seq.toList
