let base_dir = "/home/dylan/Desktop/Sesh Import"
let full_path year = Printf.sprintf "%s/%d.csv" base_dir year

let read_csv path =
  let ic = open_in path in
  let csv = Csv.of_channel ~has_header:true ic in
  let rows = Csv.input_all csv in
  close_in ic;
  rows
;;

let years =
  [ 2012; 2013; 2014; 2015; 2016; 2017; 2018; 2019; 2020; 2021; 2022; 2023; 2024; 2025 ]
;;

let () =
  let all_records =
    years
    |> List.map (fun year ->
      let rows = read_csv (full_path year) in
      let parsed = Importer.Parsers.parse_file ~schema:year rows in
      Printf.printf "Year %d: %d records\n" year (List.length parsed);
      parsed)
    |> List.flatten
  in
  Printf.printf "Total: %d records\n" (List.length all_records);
  Importer.Db.insert_all all_records
;;
