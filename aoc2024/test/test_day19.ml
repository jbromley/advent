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
let test_count_arrangements _ = assert_equal 16 (count_arrangements patterns designs)
    
let suite_name = "Day 19: Linen Layout"
let suite =
  suite_name >::: [
    "test count possible designs" >:: test_count_possible_designs;
    "test count arrangements" >:: test_count_arrangements
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
