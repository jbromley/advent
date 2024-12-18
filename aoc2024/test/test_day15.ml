open OUnit2
open Aoc
open Day15

let input_small =
  {|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|} |> (fun w -> parse_warehouse_and_dir_list w Warehouse.of_string)
                  
let input_large_str =
  {|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|}

let input_large = parse_warehouse_and_dir_list input_large_str Warehouse.of_string
let input_large_2 = parse_warehouse_and_dir_list input_large_str Warehouse2.of_string
  
    
let test_sum_coords_small _ = assert_equal 2028 (Warehouse.sum_all_gps input_small)
let test_sum_coords_large _ = assert_equal 10092 (Warehouse.sum_all_gps input_large)
let test_sum_coords_doubled _ = assert_equal 9021 (Warehouse2.sum_all_gps input_large_2)
         
let suite_name = "Day 15: Warehouse Woes"
let suite =
  suite_name >::: [
    "test sum coordinates (small input)" >:: test_sum_coords_small;
    "test sum coordinates (large input)" >:: test_sum_coords_large;
    "test sum coordinates (doubled map)" >:: test_sum_coords_doubled
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
