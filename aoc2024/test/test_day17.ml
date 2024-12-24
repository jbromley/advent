open OUnit2
open Aoc
open Day17

let input1 =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0|} |> of_string

(* let input2 = *)
(*   {|Register A: 2024 *)
(* Register B: 0 *)
(* Register C: 0 *)
(*  *)
(* Program: 0,3,5,4,3,0|} |> of_string *)
                  

let test_debug_output _ = assert_equal "4,6,3,5,6,3,5,2,1,0" (exec input1)
(* let test_find_quine _ = assert_equal 117440 (find_quine input2) *)
    
let suite_name = "Day 17: Chronospatial Computer"
let suite =
  suite_name >::: [
    "test debug output" >:: test_debug_output;
    (* "test finding quine" >:: test_find_quine *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
