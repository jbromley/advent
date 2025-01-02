open OUnit2
open Aoc
open Day25

let keys, locks = {|#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####|} |> of_string

let test_count_matches _ = assert_equal 3 (count_matches keys locks)
  
let suite_name = "Day 25: Code Chronicle"
let suite =
  suite_name >::: [
    "test count matches" >:: test_count_matches;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
