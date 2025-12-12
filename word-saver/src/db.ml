module StringMap = Map.Make(String)
open Model

type db = word_entry StringMap.t

let empty = StringMap.empty

let add_word w e db = StringMap.add w e db
let search_exact w db = StringMap.find_opt w db

let search_prefix prefix db =
  StringMap.bindings db
  |> List.filter (fun (w, _) -> String.starts_with ~prefix w)

let filter_by_tag tag db =
  StringMap.bindings db
  |> List.filter (fun (_, e) -> List.exists ((=) tag) e.tags)

let list_alphabetical db = StringMap.bindings db
let list_by_date db =
  List.sort (fun (_, a) (_, b) -> compare a.date_added b.date_added)
    (StringMap.bindings db)