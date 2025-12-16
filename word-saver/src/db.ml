module StringMap = Map.Make(String)
open Model

type db = word_entry StringMap.t

let empty = StringMap.empty

(* grab date-added when creating a word entry*)
let today () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday

(* initialize randomness *)
let () = Random.self_init ()

(* add a word, with the meaning, tags and date added information *)
let add_word word meaning tags db =
  let entry =
    {
      meaning;
      tags;
      date_added = today ();
    }
  in
  StringMap.add word entry db

(* removes a word from the database
   if the word does not exist, the database is unchanged *)
let delete_word word db =
  StringMap.remove word db

(* helper functions for review mode *)
(* shuffle word entries *)
let shuffle lst =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

(* random selection of word entries*)
let select_random n db =
  let entries = StringMap.bindings db in
  shuffle entries |> List.take n

let min3 a b c =
  min a (min b c)

(* levenshtein distance for fuzzy matching in search
  computes the min number of single-char edits
  needed to transform string s into string t *)
let levenshtein s t =
  let m = String.length s in
  let n = String.length t in

  (* dynamic programming table:
    dp.(i).(j) = edit distance between
    first i characters of s and first j characters of t *)
  let dp = Array.make_matrix (m + 1) (n + 1) 0 in

  (* base cases:
    transforming a string of length i into an empty string
    requires i deletions, and vice versa *)
  for i = 0 to m do dp.(i).(0) <- i done;
  for j = 0 to n do dp.(0).(j) <- j done;

  (* fill the table row by row*)
  for i = 1 to m do
    for j = 1 to n do
      (*cost is 0 if characters match, 1 otherwise*)
      let cost = if s.[i - 1] = t.[j - 1] then 0 else 1 in
      dp.(i).(j) <-
        min3
          (dp.(i - 1).(j) + 1)      (* deletion *)
          (dp.(i).(j - 1) + 1)      (* insertion *)
          (dp.(i - 1).(j - 1) + cost) (* substitution *)
    done
  done;

  (* final edit distance between full strings *)
  dp.(m).(n)

(* fuzzy search, max distance of 2 to avoid noisy results*)
let search_fuzzy ?(max_distance = 2) query db =
  StringMap.bindings db
  |> List.filter (fun (w, _) ->
      levenshtein query w <= max_distance)

(* exact search *)
let search_exact w db = StringMap.find_opt w db

(* prefix search *)
let search_prefix prefix db =
  StringMap.bindings db
  |> List.filter (fun (w, _) -> String.starts_with ~prefix w)

(* combined search, search priority: exact, prefix, fuzzy search *)
let search query db =
  match search_exact query db with
  | Some e ->
      [ (query, e) ]
  | None ->
      let prefix_results = search_prefix query db in
      if prefix_results <> [] then
        prefix_results
      else
        search_fuzzy query db

(* return all database entries with the given tag*)
let filter_by_tag tag db =
  StringMap.bindings db
  |> List.filter (fun (_, e) -> List.exists ((=) tag) e.tags)

(* Return a sorted list of all unique tags in the database *)
let all_tags db =
  StringMap.bindings db
  |> List.map (fun (_, e) -> e.tags)
  |> List.concat
  |> List.sort_uniq String.compare

(* returns all entries in alphabetical order *)
let list_alphabetical db = StringMap.bindings db

(* returns all entries sorted by date added (earliest to latest) *)
let list_by_date db =
  List.sort (fun (_, a) (_, b) -> compare a.date_added b.date_added)
    (StringMap.bindings db)
