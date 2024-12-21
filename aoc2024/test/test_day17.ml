open OUnit2
open Utils
open Aoc
open Day17

let input =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|} |> of_string
                  

let test_debug_output _ = assert_equal "4,6,3,5,6,3,5,2,1,0" (debug input)
(* let test_lowest_score_large _ = assert_equal 11048 (find_lowest_score_path input_large) *)
    
let suite_name = "Day 17: Chronospatial Computer"
let suite =
  suite_name >::: [
    "test debug output" >:: test_debug_output;
    (* "lowest score (large input)" >:: test_lowest_score_large; *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
