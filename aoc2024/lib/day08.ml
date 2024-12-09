(* Day 8: Resonant Collinearity *)

module CharMap = Map.Make(Char)

module Pos = struct
  type t = int * int

  let make r c : t = (r, c)

  let zero : t = (0, 0)
                    
  let ( + ) (r1, c1) (r2, c2) : t = (r1 + r2, c1 + c2)
  
  let ( - ) (r1, c1) (r2, c2) : t = (r1 - r2, c1 - c2)
  
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | other -> other

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
let on_map (px, py) (max_x, max_y) =
  px >= 0 && px <=max_x && py >= 0 && py <= max_y
                
let find_antinodes (a1 : Pos.t) (a2 : Pos.t) (max_pos : Pos.t) (resonant : bool): Pos.t list =
  let delta = Pos.(a1 - a2) in
  if not resonant then
    [Pos.(a1 + delta); Pos.(a2 - delta)] |> List.filter (fun pos -> on_map pos max_pos)
  else
    let delta' = Pos.(Pos.zero - delta) in 
    let rec loop a d =
      if not (on_map a max_pos) then []
      else a :: loop Pos.(a + d) d
    in
    loop a1 delta @ loop a2 delta'

let rec find_list_antinodes ant_list max_pos resonant =
  match ant_list with
  | [] -> []
  | a :: other_as ->
    let pairs = List.map (fun a2 -> find_antinodes a a2 max_pos resonant) other_as |> List.flatten in
    pairs @ find_list_antinodes other_as max_pos resonant

let count_antinodes (m, max_pos) resonant =
  let antinodes = CharMap.fold
      (fun _freq antennas antinodes -> find_list_antinodes antennas max_pos resonant |>
                                       (List.fold_left (fun antinodes a -> PosSet.add a antinodes) antinodes)) m PosSet.empty in
  antinodes |> PosSet.cardinal

let run () =
  let antenna_map = read_input "./input/08.txt" |> parse_map in
  Printf.printf "Day 8: Resonant Collinearity\n";
  Printf.printf "number of antinodes = %d\n" (count_antinodes antenna_map false);
  Printf.printf "number of resonant antinodes = %d\n" (count_antinodes antenna_map true)
