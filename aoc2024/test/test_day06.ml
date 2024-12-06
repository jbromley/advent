open OUnit2
open Aoc

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
  assert_equal 41 (Day06.count_visited_locations m)

let test_count_possible_obstructions _ =
  assert_equal 6 (Day06.count_possible_obstructions m)

let suite =
  "Test Day 06" >::: [
    "count visited locations" >:: test_count_visited_locations;
    "count possible obstructions" >:: test_count_possible_obstructions;
  ]

let () =
  run_test_tt_main suite
              
