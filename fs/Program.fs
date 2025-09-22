module Importer

open Parsers
open FSharp.Data

let fullPath (year: string) =
    let dir = "/home/dylan/Desktop/Sesh Import"
    dir + $"/{year}.csv"


[<EntryPoint>]
let main _ =
    let years = [ 2025 ]

    let normalized =
        years |> Seq.map string |> Seq.map (fun year -> parseFile year (fullPath year))

    normalized |> Seq.iter (fun normalized -> printfn "%A" normalized)
    0
