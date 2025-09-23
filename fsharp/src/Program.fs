module Importer

open Parsers

let parseCsvData () =
    let fullPath (year: int) =
        let dir = "/home/dylan/Desktop/Sesh Import"
        dir + $"/{year}.csv"

    seq { 2012..2025 }
    |> Seq.map (fun year -> parseFile year (fullPath year))
    |> List.concat

[<EntryPoint>]
let main _ =
    let normalized = parseCsvData ()

    let kites =
        normalized
        |> List.map (fun n -> n, Gear.boardIds n)
        |> List.iter (fun (n, k) -> printfn $"{n.Date} -- {n.Sport} {k}")
    // Looks good!

    0
