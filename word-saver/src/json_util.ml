open Model

module StringMap = Map.Make(String)

let entry_to_json e =
  `Assoc [
    ("meaning", `String e.meaning);
    ("tags", `List (List.map (fun t -> `String t) e.tags));
    ("date_added", `String e.date_added)
  ]

let db_to_json db =
  `List (
    StringMap.bindings db
    |> List.map (fun (w, e) -> `Assoc [("word", `String w); ("entry", entry_to_json e)])
  )

let entry_of_json js =
  let open Yojson.Safe.Util in
  {
    meaning = js |> member "meaning" |> to_string;
    tags = js |> member "tags" |> to_list |> List.map to_string;
    date_added = js |> member "date_added" |> to_string;
  }


let db_of_json js =
  let open Yojson.Safe.Util in
  js |> to_list |> List.fold_left (fun acc item ->
    let w = item |> member "word" |> to_string in
    let e = item |> member "entry" |> entry_of_json in
    StringMap.add w e acc
  ) StringMap.empty


let save_db file db =
  let oc = open_out file in
  Yojson.Safe.pretty_to_channel oc (db_to_json db);
  close_out oc

let load_db file =
  try Yojson.Safe.from_file file |> db_of_json
  with _ -> StringMap.empty
