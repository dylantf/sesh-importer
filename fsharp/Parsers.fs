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
    { date: DateTime
      sport: Sport
      hours: double
      windAvg: int option
      windGust: int option
      kiteSize: string list option
      wingSize: string list option
      seshType: SeshType option
      boardType: BoardType list option
      foil: string list option
      board: string list option
      location: string option
      comments: string option }

let maybeString (s: string) : string option =
    match s.Trim() with
    | "" -> None
    | s -> Some s

let parseInt = maybeString >> Option.map int

let parseMany (s: string) =
    s
    |> maybeString
    |> Option.map (fun s -> s.Split ',' |> Array.map (fun s -> s.Trim()))
    |> Option.map Array.toList

let parseSport =
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

let parseSeshType =
    function
    | Some "Spot" -> Some Spot
    | Some "Downwinder" -> Some Downwinder
    | Some "Roundwinder" -> Some Roundwinder
    | _ -> None

let parseBoardType =
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
let normalizeKiteSize (kites: string list option) =
    kites |> Option.map (List.map (fun kite -> kite.Replace("m", "")))

let parse2012 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Lull (kn)" |> parseInt
      windGust = row.GetColumn "Gust (kn)" |> parseInt
      kiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      wingSize = None
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = None
      foil = None
      board = None
      location = None
      comments = row.GetColumn "Comments" |> maybeString }

let parse2013 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Lull" |> parseInt
      windGust = row.GetColumn "Gust" |> parseInt
      kiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      wingSize = None
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = None
      foil = None
      board = None
      location = None
      comments = row.GetColumn "Comments" |> maybeString }

let parse2014 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Day" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Lull (kn)" |> parseInt
      windGust = row.GetColumn "Gust (kn)" |> parseInt
      kiteSize = row.GetColumn "Kite Size" |> parseMany |> normalizeKiteSize
      wingSize = None
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = None
      foil = None
      board = None
      location = row.GetColumn "Location" |> maybeString
      comments = row.GetColumn "Comments" |> maybeString }

let parse2016 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Lull (kts)" |> parseInt
      windGust = row.GetColumn "Gust (kts)" |> parseInt
      kiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      wingSize = None
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = row.GetColumn "Board" |> parseMany |> Option.map (List.map parseBoardType)
      foil = None
      board = None
      location = row.GetColumn "Location" |> maybeString
      comments = row.GetColumn "Comments" |> maybeString }

let parse2022 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Avg (kts)" |> parseInt
      windGust = row.GetColumn "Gust (kts)" |> parseInt
      kiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      wingSize = None
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      foil = row.GetColumn "Foil" |> parseMany
      board = row.GetColumn "Foil Board" |> parseMany
      location = row.GetColumn "Location" |> maybeString
      comments = row.GetColumn "Comments" |> maybeString }

let parse2024 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Avg (kts)" |> parseInt
      windGust = row.GetColumn "Gust (kts)" |> parseInt
      kiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      wingSize = row.GetColumn "Wing" |> parseMany
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      foil = row.GetColumn "Foil" |> parseMany
      board = row.GetColumn "Foil Board" |> parseMany
      location = row.GetColumn "Location" |> maybeString
      comments = row.GetColumn "Comments" |> maybeString }

let parse2025 (row: CsvRow) : Normalized =
    { date = row.GetColumn "Date" |> DateTime.Parse
      sport = row.GetColumn "Sport" |> parseSport
      hours = row.GetColumn "Hours" |> double
      windAvg = row.GetColumn "Avg (kts)" |> parseInt
      windGust = row.GetColumn "Gust (kts)" |> parseInt
      kiteSize = row.GetColumn "Kite" |> parseMany |> normalizeKiteSize
      wingSize = row.GetColumn "Wing" |> parseMany
      seshType = row.GetColumn "Type" |> maybeString |> parseSeshType
      boardType = row.GetColumn "Board Type" |> parseMany |> Option.map (List.map parseBoardType)
      foil = row.GetColumn "Foil" |> parseMany
      board = row.GetColumn "Board" |> parseMany
      location = row.GetColumn "Location" |> maybeString
      comments = row.GetColumn "Comments" |> maybeString }

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
