open OUnit2
open Aoc
open Day10

let input = [ "89010123";
              "78121874";
              "87430965";
              "96549874";
              "45678903";
              "32019012";
              "01329801";
              "10456732" ]

(*
let test_by_blocks _ = assert_equal 1928 (defragment fs ~by_blocks:true () |> checksum)

let test_by_files _ = assert_equal 2858 (defragment fs ~by_blocks:false () |> checksum)
*)
         
let suite_name = "Day 10: Hoof It"
let suite =
  suite_name >::: [
    (* "test defragment by blocks" >:: test_by_blocks; *)
    (* "test defragment by files" >:: test_by_files; *)
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
