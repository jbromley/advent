open OUnit2
open Utils
open Aoc
open Day12

let map = {|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|} |> Board.of_string
    
let test_calculate_fence_cost _ = assert_equal 1930 (calculate_fence_cost map)

(* let test_count_stones_fast _ = assert_equal 55312 (count_stones input 25) *)
         
let suite_name = "Day 12: Garden Groups"
let suite =
  suite_name >::: [
    "test calculating fence cost)" >:: test_calculate_fence_cost;
    (* "test counting stones (fast cache)" >:: test_count_stones_fast; *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
