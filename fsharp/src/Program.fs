module Importer

open Parsers

let parseCsvData () =
    let fullPath (year: int) =
        let dir = "/home/dylan/Desktop/Sesh Import"
        dir + $"/{year}.csv"

    seq { 2012..2026 }
    |> Seq.map (fun year -> parseFile year (fullPath year))
    |> List.concat

[<EntryPoint>]
let main _ =
    parseCsvData () |> Db.insertAll
    0
