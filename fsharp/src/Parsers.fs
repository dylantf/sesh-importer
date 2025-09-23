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
      Board: string list option
      Location: string option
      Comments: string option }

let private maybeString (s: string) : string option =
    match s.Trim() with
    | "" -> None
    | s -> Some s

let private parseInt = maybeString >> Option.map int

let private parseMany (s: string) =
    s
    |> maybeString
    |> Option.map (fun s -> s.Split ',' |> Array.map (fun s -> s.Trim()))
    |> Option.map Array.toList

let private parseSport =
    function
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

let private parseSeshType =
    function
    | Some "Spot" -> Some Spot
    | Some "Downwinder" -> Some Downwinder
    | Some "Roundwinder" -> Some Roundwinder
    | _ -> None

let private parseBoardType =
    function
    | "Twintip"
    | "Twintp" -> Twintip // Fix typo
    | "Hydrofoil" -> Hydrofoil
    | "Surfboard"
    | "Strapless" -> Surfboard
    | "SUP" -> BoardType.SUP
    | "Skis" -> Skis
    | "Snowboard" -> Snowboard
    | "Skim" -> Other
    | other -> failwith $"Unhandled board type: `{other}`"

// Some kite sizes are defined as "12m" and others just "12"
let private normalizeKiteSize (kites: string list option) =
    kites |> Option.map (List.map (fun kite -> kite.Replace("m", "")))

let private parse2012 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kn)" |> parseInt
      WindGust = row.GetColumn "Gust (kn)" |> parseInt
      KiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = None
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2013 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull" |> parseInt
      WindGust = row.GetColumn "Gust" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = None
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2014 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Day" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kn)" |> parseInt
      WindGust = row.GetColumn "Gust (kn)" |> parseInt
      KiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = None
      Foil = None
      Board = None
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2016 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Lull (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = row.GetColumn "Board" |> parseMany |> Option.map (List.map parseBoardType)
      Foil = None
      Board = None
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2022 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = None
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Foil Board" |> parseMany
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2024 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = row.GetColumn "Wing" |> parseMany
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Foil Board" |> parseMany
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let private parse2025 (row: CsvRow) : Normalized =
    { Date = row.GetColumn "Date" |> DateTime.Parse
      Sport = row.GetColumn "Sport" |> parseSport
      Hours = row.GetColumn "Hours" |> double
      WindAvg = row.GetColumn "Avg (kts)" |> parseInt
      WindGust = row.GetColumn "Gust (kts)" |> parseInt
      KiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      WingSize = row.GetColumn "Wing" |> parseMany
      SeshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      BoardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      Foil = row.GetColumn "Foil" |> parseMany
      Board = row.GetColumn "Board" |> parseMany
      Location = row.GetColumn "Location" |> maybeString
      Comments = row.GetColumn "Comments" |> maybeString }

let parseFile (schema: int) (path: string) =
    CsvFile.Load(path).Rows
    |> Seq.map (
        match schema with
        | 2012 -> parse2012
        | 2013
        | 2015 -> parse2013
        | 2014 -> parse2014
        | 2016
        | 2017
        | 2018
        | 2019
        | 2020
        | 2021 -> parse2016
        | 2022
        | 2023 -> parse2022
        | 2024 -> parse2024
        | 2025 -> parse2025
        | other -> failwith $"Schema for `{other}` not defined yet!"
    )
    |> Seq.toList
