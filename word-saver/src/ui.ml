open Model
open Json_util

let prompt text = print_string text; read_line ()

let print_entry (word,e) =
  Printf.printf "\nWord: %s\nMeaning: %s\nTags: %s\nDate: %s\n"
    word e.meaning (String.concat ", " e.tags) e.date_added

(* loop program until exiting *)
let rec loop filename db =
  print_endline "\n--- Word Saver ---";
  print_endline "1. Add word";
  print_endline "2. Delete word";
  print_endline "3. Search";
  print_endline "4. Filter by tag";
  print_endline "5. List alphabetical";
  print_endline "6. List by date";
  print_endline "7. Save & Exit";
  print_string "> ";
  match read_line () with
  | "1" ->
    let w = prompt "Word: " in
    let m = prompt "Meaning: " in
    let t = prompt "Tags (comma separated): " |> String.split_on_char ',' |> List.map String.trim in
    loop filename (Db.add_word w m t db)
  | "2" ->
    let w = prompt "Word to delete: " in
    if Db.search_exact w db <> None then (
      print_endline "Word deleted.";
      loop filename (Db.delete_word w db)
    ) else (
      print_endline "Word not found.";
      loop filename db
    )
  | "3" ->
    let w = prompt "Search: " in
    Db.search w db |> List.iter print_entry;
    loop filename db
  | "4" ->
    let t = prompt "Tag: " in
    Db.filter_by_tag t db |> List.iter print_entry;
    loop filename db
  | "5" ->
    Db.list_alphabetical db |> List.iter print_entry;
    loop filename db
  | "6" ->
    Db.list_by_date db |> List.iter print_entry;
    loop filename db
  | "7" -> save_db filename db; print_endline "Saved";
  | _ -> print_endline "Invalid option"; loop filename db