open OUnit2
open Aoc
open Day20

let track =
  {|###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############|} |> of_string

let cheats = [
  (2, 14); (4, 14); (6, 2); (8, 4); (10, 2); (12, 3); (20, 1); (36, 1); (38, 1);
  (40, 1); (64, 1)
]

let cheats_long = [
  (50, 32); (52, 31); (54, 29); (56, 39); (58, 25); (60, 23); (62, 20); (64, 19);
  (66, 12); (68, 14); (70, 12); (72, 22); (74, 4); (76, 3)
]

let cheats_to_list cheats =
  Hashtbl.to_seq cheats
  |> List.of_seq
  |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)

let test_count_cheats _ = assert_equal cheats (find_cheats track 2 2 |> cheats_to_list)
let test_count_cheats_long _ = assert_equal cheats_long (find_cheats track 20 50 |> cheats_to_list)
    
let suite_name = "Day 20: Race Condition"
let suite =
  suite_name >::: [
    "test count cheats" >:: test_count_cheats;
    "test count long cheats" >:: test_count_cheats_long
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
