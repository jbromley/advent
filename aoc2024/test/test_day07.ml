open OUnit2
open Aoc
open Day07

let eqs = List.map parse_equation ["190: 10 19";
                                   "3267: 81 40 27";
                                   "83: 17 5";
                                   "156: 15 6";
                                   "7290: 6 8 6 15";
                                   "161011: 16 10 13";
                                   "192: 17 8 14";
                                   "21037: 9 7 18 13";
                                   "292: 11 6 16 20"]

let test_sum_2_op_equations _ =
  assert_equal 3749 (sum_equations eqs false)

let test_sum_3_op_equations _ =
  assert_equal 11387 (sum_equations eqs true)

let suite_name = "Day 7: Bridge Repair"
let suite =
  suite_name >::: [
    "sum valid 2-op equations" >:: test_sum_2_op_equations;
    "sum valid 3-op equations" >:: test_sum_3_op_equations;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
