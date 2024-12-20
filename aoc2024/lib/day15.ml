(* Day 15: Warehouse Woes *)
open Utils

module CoordSet = Set.Make(Coord)
module CoordMap = Map.Make(Coord)
    
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

  let sum_all_gps (warehouse, dir_list) =
    step_all warehouse dir_list |> sum_gps_coords
end

module Warehouse2 = struct
  type t = { walls : CoordSet.t;
             boxes : Coord.t CoordMap.t;
             pos: Coord.t }

  let at { walls; boxes; _ } pos =
    if CoordSet.mem pos walls then Wall
    else if CoordMap.mem pos boxes then Box
    else Empty

  let list_fold_lefti (f : int -> 'acc -> 'a -> 'acc) (init : 'acc) (l : 'a list) : 'acc =
    let rec aux index acc = function
      | [] -> acc
      | x :: xs -> aux (index + 1) (f index acc x) xs
    in 
    aux 0 init l

  let of_string s =
    let s =
      String.split_on_char '\n' s |>
      List.map (fun s -> String.to_seq s |> List.of_seq) |>
      List.map
        (fun line ->
           List.map (function
               | '#' -> ['#'; '#']
               | 'O' -> ['['; ']']
               | '.' -> ['.'; '.']
               | '@' -> ['@'; '.']
               | _ -> failwith "of_string: invalid character") line |> List.concat)
    in
    let walls, boxes, pos =
      list_fold_lefti
        (fun y acc line ->
           list_fold_lefti
             (fun x (ws, bs, pos) ch ->
                match ch with
                | '#' -> (CoordSet.add (x, y) ws, bs, pos)
                | '@' -> (ws, bs, (x, y))
                | '[' ->
                  let l = (x, y) in
                  let r = (x + 1, y) in
                  (ws, CoordMap.add l r bs |> CoordMap.add r l, pos)
                | '.' | ']' -> (ws, bs, pos)
                | _ -> failwith "of_string: invalid character")
             acc
             line)
        (CoordSet.empty, CoordMap.empty, (0, 0))
        s
    in
    { walls; boxes; pos }

  let to_string { boxes; walls; pos = (x, y) } =
    let xmax, ymax =
      CoordSet.fold (fun (x, y) (xmin, ymin) -> (max x xmin, max y ymin)) walls (min_int, min_int)
    in
    let board = Array.make_matrix (ymax + 1) (xmax + 1) '.' in
    board.(y).(x) <- '@';
    CoordSet.iter (fun (x, y) -> board.(y).(x) <- '#') walls;
    CoordMap.iter
      (fun (x, y) (xn, _) ->
         let xleft = if x < xn then x else xn in
         board.(y).(xleft) <- '[';
         board.(y).(xleft + 1) <- ']')
      boxes;
    Board.to_string board

  let coord_set_to_string s =
    let coords =
      List.map
        (fun elt -> Printf.sprintf "%s" (Coord.to_string elt))
        (CoordSet.to_list s)
    in
    Printf.sprintf "{ %s }" (String.concat "; " coords)

  let rec update_boxes boxes moved_boxes dir =
    if CoordSet.cardinal moved_boxes = 0 then boxes
    else
      let pos = CoordSet.choose moved_boxes in
      let neighbor = CoordMap.find pos boxes in 
      let m' = CoordMap.remove pos boxes 
               |> CoordMap.remove neighbor
               |> CoordMap.add (Coord.add pos dir) (Coord.add neighbor dir)
               |> CoordMap.add (Coord.add neighbor dir) (Coord.add pos dir) in 
      update_boxes m' (CoordSet.remove pos moved_boxes |> CoordSet.remove neighbor) dir
    
  let move_boxes ({ walls; boxes; pos } as t) dir =
    let rec get_moved_boxes new_boxes moved_boxes =
      (* Check if the new boxes can be moved. *)
      let next_cells = CoordSet.map (fun cell -> Coord.add cell dir) new_boxes
                     |> CoordSet.filter (fun cell -> not (CoordSet.mem cell new_boxes)) in
      Printf.printf "next_cells = %s\n" (coord_set_to_string next_cells);
      if CoordSet.exists (fun cell -> at t cell = Wall) next_cells then
        (* Wall in the way, no boxes can move. *)
        None
      else if CoordSet.for_all (fun cell -> at t cell = Empty) next_cells then
        (* Free path and no more boxes, move all the boxes. *)
        Some (CoordSet.union moved_boxes new_boxes)
      else
        (* Check for another layer of boxes. Look at all next cells and
           if it is a box, add them to new boxes for next recursion. *)
        let next_boxes =
          CoordSet.fold
            (fun cell acc ->
               match CoordMap.find_opt cell boxes with
               | None -> acc
               | Some neighbor -> CoordSet.union acc (CoordSet.of_list [cell; neighbor]))
            next_cells
            CoordSet.empty
        in
        Printf.printf "next boxes = %s\n" (coord_set_to_string next_boxes);
        get_moved_boxes next_boxes (CoordSet.union moved_boxes new_boxes)
    in
    let next_pos = Coord.add pos dir in
    let initial_boxes = CoordSet.of_list [next_pos; CoordMap.find next_pos boxes]
    in
    Printf.printf "initial boxes = %s\n" (coord_set_to_string initial_boxes);
    let moved_boxes = get_moved_boxes initial_boxes CoordSet.empty in
    Printf.printf "moved boxes = %s\n" (coord_set_to_string (Option.value moved_boxes ~default:CoordSet.empty));
    match moved_boxes with
    | None -> t
    | Some moved_boxes ->
      let boxes = update_boxes boxes moved_boxes dir in
      { walls; boxes; pos = next_pos }
    
  let step ({ pos; _ } as t) dir =
    let next_pos = Coord.add pos dir in
    match at t next_pos with
    | Empty -> { t with pos = next_pos }
    | Wall -> t
    | Box -> move_boxes t dir

  let step_all t dir_list =
    List.fold_left step t dir_list

  let sum_gps_coords { boxes; _ } =
    CoordMap.fold
      (fun (x1, y1) (x2, y2) acc ->
         let x, y = if x1 < x2 then (x1, y1) else (x2, y2) in
         acc + (100 * y + x) / 2)
      boxes
      0

  let sum_all_gps (warehouse, dir_list) =
    step_all warehouse dir_list |> sum_gps_coords
  
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
    
let parse_warehouse_and_dir_list s of_string =
  let ss = Str.global_replace (Str.regexp_string "\n\n") "\030" s |> String.split_on_char '\030' in
  let board_str = List.nth ss 0 in
  let dir_str = List.nth ss 1 in
  (of_string board_str, parse_dir_list dir_str)

let run () =
  let s = Io.read_file "./input/15.txt" in
  let input1 = parse_warehouse_and_dir_list s Warehouse.of_string in
  let input2 = parse_warehouse_and_dir_list s Warehouse2.of_string in
  Printf.printf "Day 15: Warehouse Woes\n";
  Printf.printf "sum of GPS coordinates = %d\n" (Warehouse.sum_all_gps input1);
  Printf.printf "sum of GPS coordinates (doubled map) = %d\n" (Warehouse2.sum_all_gps input2);

