open OUnit2
open Aoc
open Day21

let _codes =
  {|029A
980A
179A
456A
379A|} |> of_string

(* let test_count_cheats _ = assert_equal 44 (count_cheats track 2 2) *)
(* let test_count_cheats_long _ = assert_equal 285 (count_cheats track 20 50) *)
    
let suite_name = "Day 21: Keypad Conundrum"
let suite =
  suite_name >::: [
    (* "test count cheats" >:: test_count_cheats; *)
    (* "test count long cheats" >:: test_count_cheats_long *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
