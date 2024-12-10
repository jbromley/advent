open OUnit2
open Aoc
open Day09

let input = "2333133121414131402"


let test_checksum _ = assert_equal 1928 (checksum input)


(* let test_count_resonant_antinodes _ = assert_equal 34 (count_antinodes map true) *)

let suite_name = "Day 9: Disk Fragmenter"
let suite =
  suite_name >::: [
    "test checksum" >:: test_checksum;
    (* "count resonant antinodes" >:: test_count_resonant_antinodes; *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
