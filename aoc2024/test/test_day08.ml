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

let test_count_antinodes _ = assert_equal 14 (count_antinodes map false)

let test_count_resonant_antinodes _ = assert_equal 34 (count_antinodes map true)

let suite_name = "Day 8: Resonant Collinearity"
let suite =
  suite_name >::: [
    "count antinodes" >:: test_count_antinodes;
    "count resonant antinodes" >:: test_count_resonant_antinodes;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
