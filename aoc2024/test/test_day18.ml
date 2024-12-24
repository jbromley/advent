open OUnit2
open Aoc
open Day18

let drops =
  {|5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0|} |> of_string

let test_minimum_steps_12 _ = assert_equal 22 (minimum_steps drops (6, 6) 12)
let test_find_path_blocker _ = assert_equal (6, 1) (find_path_blocker drops (6, 6))
    
let suite_name = "Day 18: RAM Run"
let suite =
  suite_name >::: [
    "test minimum steps (12 ns)" >:: test_minimum_steps_12;
    "test blocking drop" >:: test_find_path_blocker
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
