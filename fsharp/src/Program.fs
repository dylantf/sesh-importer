module Importer

open Parsers
open System
open System.IO

let parseCsvData () =
    let fullPath (year: int) =
        let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
        Path.Combine(home, "Desktop", "Sesh Import", $"{year}.csv")

    seq { 2012..2026 }
    |> Seq.map (fun year -> parseFile year (fullPath year))
    |> List.concat

[<EntryPoint>]
let main _ =
    parseCsvData () |> Db.insertAll
    0
