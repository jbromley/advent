open OUnit2
open Aoc
open Day06    

let m = [|[|'.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '#'; '.'; '.'; '^'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'|];
          [|'#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'|];
          [|'.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'|]|]

let test_count_visited_locations _ =
  assert_equal 41 (count_visited_locations m)

let test_count_possible_obstructions _ =
  assert_equal 6 (count_possible_obstructions m)

let suite_name = "Day 06: Gallivanting Guard"
let suite =
  suite_name >::: [
    "count visited locations" >:: test_count_visited_locations;
    "count possible obstructions" >:: test_count_possible_obstructions;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
              
