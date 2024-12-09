(* Day 8: Resonant Collinearity *)

module CharMap = Map.Make(Char)

module Pos = struct
  type t = int * int

  (* let add (r1, c1) (r2, c2) = *)
  (*   (r1 + r2, c1 + c2) *)
  (*  *)
  (* let sub (r1, c1) (r2, c2) = *)
  (*   (r1 - r2, c1 - c2) *)
  
  let compare (x1, y1) (x2, y2) =
    match (compare x1 x2, compare y1 y2) with
    | (-1, -1) | (-1, 0) | (0, -1) -> -1
    | (0, 0) -> 0
    | (1, _) | (_, 1) -> 1
    | _ -> failwith "Pos.compare: invalid comparison"
end

module PosSet = Set.Make(Pos)

(** Read the input file and return [string list]. *)
let read_input filename =
  let ic = open_in filename in
  let rec loop lines =
    try
      let line = input_line ic in
      loop (line :: lines)
    with End_of_file ->
      close_in ic;
      List.rev lines
  in
  loop []

(** Take a [string list] and parse it to extract a map of antennas of type
    [(int * int) CharMap.t] and the maximum map row and column as [int * int].
    These two items are returned as a pair. *)
let parse_map lst =
  let string_fold_lefti f acc s =
    let len = String.length s in
    let rec loop i acc =
      if i = len then acc
      else loop (i + 1) (f acc i s.[i])
    in
    loop 0 acc
  in 
  let update_map m ch pos =
    if ch = '.' then m
    else
      let current_positions = CharMap.find_opt ch m |> (Option.value ~default:[]) in
      CharMap.add ch (pos :: current_positions) m
  in
  let m = List.fold_left
    (fun m (row_idx, row) ->
       string_fold_lefti (fun m col_idx ch -> update_map m ch (row_idx, col_idx)) m row)
    CharMap.empty (List.mapi (fun row_idx row -> (row_idx, row)) lst)
  in
  (m, (List.length lst - 1, String.length (List.hd lst) - 1))

(** Determine if a given position is on the map and return a [bool]. *)
let on_map (x, y) (max_x, max_y) =
  x >= 0 && x <= max_x && y >=0 && y <= max_y
                
(** Given two antenna locations [a1] and [a2], return a list of antinode
    locations as [(int * int) list]. *)
let find_pair_antinodes (x1, y1) (x2, y2) max_pos =
  let dx, dy = (x1 - x2, y1 - y2) in
  [(x1 + dx, y1 + dy); (x2 - dx, y2 - dy)] |> List.filter (fun pos -> on_map pos max_pos)

let rec find_list_antinodes ant_list max_pos =
  match ant_list with
  | [] -> []
  | a :: other_as ->
    let pairs = List.map (fun a2 -> find_pair_antinodes a a2 max_pos) other_as |> List.flatten in
    pairs @ find_list_antinodes other_as max_pos

let count_antinodes (m, max_pos) =
  let antinodes = CharMap.fold
      (fun _freq antennas antinodes -> find_list_antinodes antennas max_pos |>
                                       (List.fold_left (fun antinodes a -> PosSet.add a antinodes) antinodes)) m PosSet.empty in
  antinodes |> PosSet.cardinal

let run () =
  let antenna_map = read_input "./input/08.txt" |> parse_map in
  Printf.printf "Day 8: Resonant Collinearity\n";
  Printf.printf "number of antinodes = %d\n" (count_antinodes antenna_map);
  (* Printf.printf "sum of valid three-op equations = %d\n" (sum_equations eqs true) *)
