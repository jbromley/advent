open OUnit2
open Aoc

let setup _test_ctxt =
  let input = [[7; 6; 4; 2; 1];
               [1; 2; 7; 8; 9];
               [9; 7; 6; 2; 1];
               [1; 3; 2; 4; 5];
               [8; 6; 4; 4; 1];
               [1; 3; 6; 7; 9]] in
  input

let teardown _input _test_ctxt =
  ()

let test_count_safe_reports input =
  assert_equal 2 (Day02.count_safe_reports input)

let test_count_safe_damped_reports input =
  assert_equal 4 (Day02.count_safe_damped_reports input)

let suite =
  "Test Day 02" >::: [
    "test safe reports" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in test_count_safe_reports input);
    "test safe damped reports" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in test_count_safe_damped_reports input)
  ]

let () =
  run_test_tt_main suite
              
