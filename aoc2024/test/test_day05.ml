open OUnit2
open Aoc

let setup _test_ctxt = Day05.parse_input ["47|53";
                                          "97|13";
                                          "97|61";
                                          "97|47";
                                          "75|29";
                                          "61|13";
                                          "75|53";
                                          "29|13";
                                          "97|29";
                                          "53|29";
                                          "61|53";
                                          "97|53";
                                          "61|29";
                                          "47|13";
                                          "75|47";
                                          "97|75";
                                          "47|61";
                                          "75|61";
                                          "47|29";
                                          "75|13";
                                          "53|13";
                                          "";
                                          "75,47,61,53,29";
                                          "97,61,53,29,13";
                                          "75,29,13";
                                          "75,97,47,61,53";
                                          "61,13,29";
                                          "97,13,75,29,47"]
    
let teardown _input _test_ctxt =
  ()

let test_sum_valid_updates (rules, updates) =
  assert_equal 143 (Day05.sum_valid_updates rules updates)

let test_sum_fixed_updates (rules, updates) =
  assert_equal 123 (Day05.sum_fixed_updates rules updates)

let suite =
  "Test Day 05" >::: [
    "sum valid updates middle page" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                                          test_sum_valid_updates input);
    "sum fixed updates middle page" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                                          test_sum_fixed_updates input);
  ]

let () =
  run_test_tt_main suite
