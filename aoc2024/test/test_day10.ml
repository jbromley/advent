open OUnit2
open Aoc
open Day10

let map = parse_map [ "89010123";
                      "78121874";
                      "87430965";
                      "96549874";
                      "45678903";
                      "32019012";
                      "01329801";
                      "10456732" ]


let test_scoring _ = assert_equal 36 (sum_trailhead_scores map ~rating:false ())

let test_rating _ = assert_equal 81  (sum_trailhead_scores map ~rating:true ())
         
let suite_name = "Day 10: Hoof It"
let suite =
  suite_name >::: [
    "test scoring" >:: test_scoring;
    "test rating" >:: test_rating;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
