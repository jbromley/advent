open OUnit2
open Aoc

let setup _test_ctxt =
  let ls = ([3; 4; 2; 1; 3; 3],
            [4; 3; 5; 3; 9; 3]) in
  ls

let teardown _ls _test_ctxt =
  ()

let test_distance (l1, l2) =
  assert_equal 11 (Day01.list_dist l1 l2)

let test_similarity (l1, l2) =
  assert_equal 31 (Day01.similarity l1 l2)

let suite_name = "Day 1: Historian Hysteria"
let suite =
  suite_name >::: [
    "test distance" >:: (fun test_ctxt -> let ls = bracket setup teardown test_ctxt in test_distance ls);
    "test similarity" >:: (fun test_ctxt -> let ls = bracket setup teardown test_ctxt in test_similarity ls)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
  
          
