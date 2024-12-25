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
  (2, 14); (4, 14); (6, 2); (8, 4); (10, 2); (12, 3); (20, 1); (36, 1); (38, 1); (40, 1); (64, 1)
]

let cheats_to_list cheats =
  Hashtbl.to_seq cheats
  |> List.of_seq
  |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)

let test_count_cheats _ = assert_equal cheats (count_cheats track |> cheats_to_list)
(* let test_count_arrangements _ = assert_equal 16 (count_arrangements patterns designs) *)
    
let suite_name = "Day 20: Race Condition"
let suite =
  suite_name >::: [
    "test count shortcuts" >:: test_count_cheats;
    (* "test count arrangements" >:: test_count_arrangements *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
