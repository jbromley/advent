open OUnit2
open Aoc
open Day14

let robots = [ "p=0,4 v=3,-3";
               "p=6,3 v=-1,-3";
               "p=10,3 v=-1,2";
               "p=2,0 v=2,-1";
               "p=0,0 v=1,3";
               "p=3,0 v=-2,-2";
               "p=7,6 v=-1,-3";
               "p=3,0 v=-1,-2";
               "p=9,3 v=2,3";
               "p=7,3 v=-1,2";
               "p=2,4 v=2,-3";
               "p=9,5 v=-3,-3]"] |> parse_input

    
let test_safety_factor _ = assert_equal 12 (safety_factor robots 100 (11, 7))
(* let test_count_stones_fast _ = assert_equal 55312 (count_stones input 25) *)
         
let suite_name = "Day 14: Restroom Redoubt"
let suite =
  suite_name >::: [
    "test calculating safety factor)" >:: test_safety_factor;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
