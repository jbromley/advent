open OUnit2
open Aoc

let setup _test_ctxt =
  let input = [|"MMMSXXMASM";
                "MSAMXMSMSA";
                "AMXSXMAAMM";
                "MSAMASMSMX";
                "XMASAMXAMM";
                "XXAMMXXAMA";
                "SMSMSASXSS";
                "SAXAMASAAA";
                "MAMMMXMMMM";
                "MXMXAXMASX"|] in 
  input
    
let teardown _input _test_ctxt =
  ()

let test_count_all_xmas input =
  assert_equal 18 (Day04.count_all_xmas input)

let test_count_all_mas_crosses input =
  assert_equal 9 (Day04.count_all_mas_crosses input)

let suite_name = "Day 4: Ceres Search "
  
let suite =
  suite_name >::: [
    "count all \"XMAS\"" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                               test_count_all_xmas input);
    "count all \"MAS\" crosses" >:: (fun test_ctxt -> let input = bracket setup teardown test_ctxt in
                                      test_count_all_mas_crosses input)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
              
