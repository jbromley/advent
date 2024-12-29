open OUnit2
open Aoc
open Day22

let test_sum_secrets _ = assert_equal 37327623 (sum_secrets 2000 [1; 10; 100; 2024])
let test_maximize_bananas _ = assert_equal 23 (maximize_bananas 2000 [1; 2; 3; 2024])
    
let suite_name = "Day 22: Monkey Market"
let suite =
  suite_name >::: [
    "test sum secrets" >:: test_sum_secrets;
    "test maximize bananas" >:: test_maximize_bananas
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
