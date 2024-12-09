open OUnit2
open Aoc
open Day08

let map = parse_map ["............";
                     "........0...";
                     ".....0......";
                     ".......0....";
                     "....0.......";
                     "......A.....";
                     "............";
                     "............";
                     "........A...";
                     ".........A..";
                     "............";
                     "............"]

let test_count_antinodes _ = assert_equal 14 (count_antinodes map)

(*
let test_sum_3_op_equations _ =
  assert_equal 11387 (sum_equations eqs true)
*)

let suite_name = "Day 8: Resonant Collinearity"
let suite =
  suite_name >::: [
    "count antinodes" >:: test_count_antinodes;
    (* "sum valid 3-op equations" >:: test_sum_3_op_equations; *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
