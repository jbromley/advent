open OUnit2
open Aoc
open Day09

let fs = "2333133121414131402" |>
         String.to_seq |>
         List.of_seq |>
         List.map (fun ch -> Char.code ch - Char.code '0') |> build_fs

let test_by_blocks _ = assert_equal 1928 (defragment fs ~by_blocks:true () |> checksum)

let test_by_files _ = assert_equal 2858 (defragment fs ~by_blocks:false () |> checksum)

let suite_name = "Day 9: Disk Fragmenter"
let suite =
  suite_name >::: [
    "test defragment by blocks" >:: test_by_blocks;
    "test defragment by files" >:: test_by_files;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
