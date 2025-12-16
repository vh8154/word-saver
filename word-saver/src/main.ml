open Json_util
open Ui

let () =
  let file = "words.json" in
  let db = load_db file in
  loop file db []
