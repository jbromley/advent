open OUnit2
open Utils
open Aoc
open Day16

let input_small =
  {|###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|} |> Board.of_string
                  
let input_large =
  {|#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
|} |> Board.of_string

let test_lowest_score_small _ = assert_equal 7036 (find_lowest_score_path input_small)
let test_lowest_score_large _ = assert_equal 11048 (find_lowest_score_path input_large)
let test_count_path_tiles_small _ = assert_equal 45 (count_path_tiles input_small)
let test_count_path_tiles_large _ = assert_equal 64 (count_path_tiles input_large)
    
(* let test_sum_coords_doubled _ = assert_equal 9021 (Warehouse2.sum_all_gps input_large_2) *)
         
let suite_name = "Day 16: Reindeer Maze"
let suite =
  suite_name >::: [
    "lowest score (small input)" >:: test_lowest_score_small;
    "lowest score (large input)" >:: test_lowest_score_large;
    "number of tiles (small input)" >:: test_count_path_tiles_small;
    "number of tiles (large input)" >:: test_count_path_tiles_large;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
