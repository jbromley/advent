open OUnit2
open Aoc
open Day19

let patterns, designs =
  {|r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb|} |> of_string

let test_count_possible_designs _ = assert_equal 6 (count_possible_designs patterns designs)
(* let test_find_path_blocker _ = assert_equal (6, 1) (find_path_blocker drops (6, 6)) *)
    
let suite_name = "Day 19: Linen Layout"
let suite =
  suite_name >::: [
    "test count possible designs" >:: test_count_possible_designs;
    (* "test blocking drop" >:: test_find_path_blocker *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
