open OUnit2
open Aoc

let setup _test_ctxt =
  let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" in
  input

let teardown _input _test_ctxt =
  ()

let test_simple_multiply_add input =
  assert_equal 161 (Day03.simple_multiply_add input)

let test_conditional_multiply_add input =
  assert_equal 48 (Day03.conditional_multiply_add input)

let suite =
  "Test Day 03" >::: [
    "test simple multiply" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                                 test_simple_multiply_add input);
    "test conditional multiply" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                                      test_conditional_multiply_add input)
  ]

let () =
  run_test_tt_main suite
              
