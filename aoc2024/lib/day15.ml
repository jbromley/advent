(* Day 15: Warehouse Woes *)
open Utils

module CoordSet = Set.Make(Coord)
    
type tile = Empty | Box | Wall

module Warehouse = struct
  type t = { board : tile Board.t; pos : Coord.t }

  let find_robot b =
    let rec aux y robot_pos =
      match Array.find_index (fun ch -> ch = '@') b.(y) with
      | Some x -> (x, y)
      | None -> aux (y + 1) robot_pos
    in
    aux 0 None
    
  let of_string s =
    let board = Board.of_string s in
    let start_pos = find_robot board in
    let warehouse =
      Array.map
        (fun row -> Array.map (function '#' -> Wall | 'O' -> Box | _ -> Empty) row)
        board
    in
    { board = warehouse; pos = start_pos }

  let size b =
    (Array.length b.(0), Array.length b)

  let at b (x, y) =  b.(y).(x)
  let at_opt b (x, y) =
    let w, h = size b in
    if 0 <= x && x < w && 0 <= y && y < h then Some b.(y).(x)
    else None

  let set b (x, y) cell = b.(y).(x) <- cell

  let move_boxes ( { board; pos } as t) dir =
    let rec find_empty_cell board pos =
      match at board pos with
      | Wall -> None
      | Empty -> Some pos
      | Box -> find_empty_cell board (Coord.add pos dir)
    in
    let next_pos = Coord.add pos dir in
    match find_empty_cell board next_pos with
    | None -> t
    | Some empty_cell ->
      set board next_pos Empty;
      set board empty_cell Box;
      { board; pos = next_pos }

  let step ({ board; pos } as t) dir =
    let next_pos = Coord.add pos dir in
    match at board next_pos with
    | Empty -> { board; pos = next_pos }
    | Wall -> t
    | Box -> move_boxes t dir

  let step_all t dir_list =
    List.fold_left step t dir_list

  let sum_gps_coords { board; _ } =
    Array.fold_left
      (fun acc_y (row, y) ->
         acc_y + Array.fold_left
           (fun acc_x (cell, x) ->
              acc_x + (match cell with
                  | Box -> 100 * y + x
                  | _ -> 0))
           0
           (Array.mapi (fun x cell -> (cell, x)) row))
      0
      (Array.mapi (fun y row -> (row, y)) board)

  let to_string { board; pos } =
    let rows = Array.mapi
        (fun row_idx row ->
           Array.mapi
             (fun col_idx tile ->
                if (col_idx, row_idx) = pos then
                  "@"
                else
                  match tile with
                  | Wall -> "#"
                  | Box -> "O"
                  | Empty -> ".")
             row
           |> Array.to_list
           |> String.concat "")
        board
               |> Array.to_list
    in
    String.concat "\n" rows
end

let parse_dir_list s =
  Str.global_replace (Str.regexp_string "\n") "" s |>
  String.to_seq |>
  Seq.map
    (function
      | '>' -> (1, 0)
      | 'v' -> (0, 1)
      | '<' -> ((-1), 0)
      | '^' -> (0, (-1))
      | _ -> failwith "parse_dir_list: bad character") |>
  List.of_seq
    
let parse_warehouse_and_dir_list s =
  let ss = Str.global_replace (Str.regexp_string "\n\n") "\030" s |> String.split_on_char '\030' in
  let board_str = List.nth ss 0 in
  let dir_str = List.nth ss 1 in
  (Warehouse.of_string board_str, parse_dir_list dir_str)

let sum_all_gps (warehouse, dir_list) =
  Warehouse.step_all warehouse dir_list |> Warehouse.sum_gps_coords

let run () =
  let input = Io.read_file "./input/15.txt" |> parse_warehouse_and_dir_list in
  Printf.printf "Day 15: Warehouse Woes\n";
  Printf.printf "sum of GPS coordinates  = %d\n" (sum_all_gps input);
  (* Printf.printf "find tree = %d\n" (find_tree robots map_size) *)
