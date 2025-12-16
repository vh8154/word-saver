open Model
open Json_util

(* cache of recent search results:
   (query, results) list where the most recent is first *)
type search_cache = (string * (string * Model.word_entry) list) list

let prompt text = print_string text; read_line ()

let print_entry (word,e) =
  Printf.printf "\nWord: %s\nMeaning: %s\nTags: %s\nDate: %s\n"
    word e.meaning (String.concat ", " e.tags) e.date_added

(* helper functions for review mode *)
(* -------------------------------- *)
let wait () =
  ignore (read_line ())

let review words =
  List.iter (fun (w, e) ->
    Printf.printf "\nWord: %s\n" w;
    print_endline "(press Enter to see meaning)";
    wait ();
    Printf.printf "Meaning: %s\nTags: %s\n"
      e.meaning
      (String.concat ", " e.tags);
    print_endline "------------------";
    wait ()
  ) words

(* helper functions for cacheing recent searches *)
(* --------------------------------------------- *)
let max_cache_size = 5

let cache_lookup query cache =
  List.assoc_opt query cache

let cache_add query results cache =
  let cache =
    cache |> List.remove_assoc query  (* avoid duplicates *)
  in
  let cache = (query, results) :: cache in
  if List.length cache > max_cache_size then
    List.rev cache |> List.tl |> List.rev
  else
    cache

(* search query history helper *)
(* --------------------------- *)
let print_recent_searches cache =
  if cache <> [] then (
    print_endline "\nRecent searches:";
    cache
    |> List.map fst
    |> List.iteri (fun i q ->
        Printf.printf "%d. %s\n" (i + 1) q)
  )

(* loop program until exiting *)
let rec loop filename db cache =
  print_endline "\n--- Word Saver ---";
  print_endline "1. Add word";
  print_endline "2. Delete word";
  print_endline "3. Search";
  print_endline "4. Filter by tag";
  print_endline "5. List alphabetical";
  print_endline "6. List by date";
  print_endline "7. Review mode";
  print_endline "8. Save & Exit";
  print_string "> ";
  match read_line () with
  | "1" ->
    let w = prompt "Word: " in
    let m = prompt "Meaning: " in
    let t = prompt "Tags (comma separated): " |> String.split_on_char ',' |> List.map String.trim in
    loop filename (Db.add_word w m t db) [] (* clear cache *)
  | "2" ->
    let w = prompt "Word to delete: " in
    if Db.search_exact w db <> None then (
      print_endline "Word deleted.";
      loop filename (Db.delete_word w db) [] (* clear cache *)
    ) else (
      print_endline "Word not found.";
      loop filename db cache
    )
  | "3" ->
    print_recent_searches cache;
    let q = prompt "Search: " in

    let results =
      match cache_lookup q cache with
      | Some results -> results
      | None -> Db.search q db
    in

    if results = [] then
      print_endline "No matches found."
    else
      List.iter print_entry results;

    let cache = cache_add q results cache in
    loop filename db cache
| "4" ->
    let tags = Db.all_tags db in
    if tags = [] then (
      print_endline "No tags available.";
      loop filename db cache
    ) else (
      print_endline "\nAvailable tags:";
      List.iter (fun t -> Printf.printf "- %s\n" t) tags;
      let t = prompt "Tag: " in
      Db.filter_by_tag t db |> List.iter print_entry;
      loop filename db cache
    )
  | "5" ->
    Db.list_alphabetical db |> List.iter print_entry;
    loop filename db cache
  | "6" ->
    Db.list_by_date db |> List.iter print_entry;
    loop filename db cache
  | "7" ->
    let n =
      prompt "How many words to review? "
      |> int_of_string_opt
    in
    (match n with
     | None ->
         print_endline "Please enter a valid number.";
         loop filename db cache
     | Some n ->
         let total = List.length (Db.list_alphabetical db) in
         if n <= 0 || n > total then (
           Printf.printf "Please choose between 1 and %d.\n" total;
           loop filename db cache
         ) else (
           let words = Db.select_random n db in
           review words;
           loop filename db cache
         ))
  | "8" -> 
    save_db filename db; 
    print_endline "Saved... exiting!"
  | _ -> print_endline "Invalid option"; loop filename db cache
