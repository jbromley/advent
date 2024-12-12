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

let start_pos = find_start m

let test_count_visited_locations _ =
  let visited, _ = visited_locations m start_pos in
  assert_equal 41 (count_visited_locations visited)

let test_count_possible_obstructions _ =
  let _, steps = visited_locations m start_pos in 
  assert_equal 6 (count_possible_obstructions m steps)

let suite_name = "Day 6: Gallivanting Guard"
let suite =
  suite_name >::: [
    "count visited locations" >:: test_count_visited_locations;
    "count possible obstructions" >:: test_count_possible_obstructions;
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
              
