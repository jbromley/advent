open OUnit2
open Aoc
open Day11

let input = "125 17" |> parse_input

let test_blink _ = assert_equal [1; 1; 3; 45; 67; 6072] (blink [0; 13; 4567; 3])
    
let test_count_stones_slow _ = assert_equal 55312 (count_stones_slow input 25)

let test_count_stones_fast _ = assert_equal 55312 (count_stones input 25)

         
let suite_name = "Day 11: Plutonian Pebbles"
let suite =
  suite_name >::: [
    "test blink" >:: test_blink;
    "test counting stones (slow)" >:: test_count_stones_slow;
    "test counting stones (fast cache)" >:: test_count_stones_fast;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
