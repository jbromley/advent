open OUnit2
open Aoc
open Day13

let input = [ "Button A: X+94, Y+34";
              "Button B: X+22, Y+67";
              "Prize: X=8400, Y=5400";
              "";
              "Button A: X+26, Y+66";
              "Button B: X+67, Y+21";
              "Prize: X=12748, Y=12176";
              "";
              "Button A: X+17, Y+86";
              "Button B: X+84, Y+37";
              "Prize: X=7870, Y=6450";
              "";
              "Button A: X+69, Y+23";
              "Button B: X+27, Y+71";
              "Prize: X=18641, Y=10279"] |> parse_input

let test_sum_win_cost _ = assert_equal 480 (sum_win_costs input 100)
         
let suite_name = "Day 13: Claw Contraption"
let suite =
  suite_name >::: [
    "test summing win cost (part 1)" >:: test_sum_win_cost
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
