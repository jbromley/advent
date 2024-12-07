(* Day 6: Guard Gallivant *)

module Pos = struct
  type t = int * int
           
  let add (r1, c1) (r2, c2) =
    (r1 + r2, c1 + c2)
    
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | other -> other
end

module PosSet = Set.Make(Pos)

type rotation = CW | CCW

(** Type describing the directions the guard can move. *)
module Direction = struct
  type t = Up | Right | Down | Left

  (** Rotate a direction clockwise by 90 degrees. *)
  let rotate_cw = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  (** Convert a direction of movement to a location offset. *)
  let offset = function
    | Up -> (-1, 0)
    | Right -> (0, 1)
    | Down -> (1, 0)
    | Left -> (0, -1)

  let to_string = function 
    | Up -> "up"
    | Right -> "right"
    | Down -> "down"
    | Left -> "left"  
end

module PosDir = struct
  type t = Pos.t * Direction.t

  let compare (l1, d1) (l2, d2) =
    match compare l1 l2 with
    | 0 -> compare d1 d2
    | other -> other

  let rotate_cw (loc, dir) =
    (loc, Direction.rotate_cw dir)

  let advance (pos, dir) =
    (Pos.add pos (Direction.offset dir), dir)
end

module PosDirSet = Set.Make(PosDir)

(** Type definition for a map. *)
module Map = struct 
  type t = char array array

  (** Get the character at the given location. Return None if the location is
      Not in the map and Some l where l is the location otherwise. *)
  let at (m : t) ((r, c) : Pos.t) =
    try Some m.(r).(c)
    with Invalid_argument _ -> None

  let set (m : t) ((r, c) : Pos.t) (ch : char) =
    let row = m.(r) in
    Array.set row c ch

  let dims (m : t) : int * int =
    (Array.length m, Array.length m.(0))
    
  (** Find all occurrences of a character in a map. Return a list of row,
      column pairs. *)
  let find (m : t) (target : char) : (int * int) list =
    let find_in_row row_index row =
      Array.fold_left (fun acc (col_index, cell) ->
          if cell = target then (row_index, col_index) :: acc else acc
        ) [] (Array.mapi (fun i x -> (i, x)) row)
    in
    Array.fold_left (fun acc (row_index, row) ->
        (find_in_row row_index row) @ acc
      ) [] (Array.mapi (fun i r -> (i, r)) m) |> List.rev
end

(** Convert a string into an array of characters. *)
let array_of_string s = String.to_seq s |> Array.of_seq

(** Read a file and return an array of strings. *)
let read_map (name : string) : Map.t =
  let ic = open_in name in 
  let try_read () =
    try
      Some(input_line ic)
    with End_of_file -> None in
  let rec loop input =
    match try_read () with
    | Some s ->
      loop (array_of_string s :: input)
    | None ->
      close_in ic; List.rev input |> Array.of_list in
  loop []

  (** Find the location where the guard starts. *)
  let find_start m =
    Map.find m '^' |> List.hd

(** Part 1: Find the number of distinct locations the guard visits before leaving the map. *)
let visited_locations (m : Map.t) (start_pos : Pos.t) : PosSet.t =
  let rec step locations pos dir =
    let next_pos = Pos.add pos (Direction.offset dir) in
    let cell = Map.at m next_pos in
    match cell with
    | None ->
      (* We've left the map, we're done. *)
      locations
    | Some '#' ->
      (* We're going to hit an obstacle, turn right. *)
      let new_dir = Direction.rotate_cw dir in
      step locations pos new_dir
    | _ ->
      (* Must be a '.' or open spot. *)
      step (PosSet.add next_pos locations) next_pos dir
  in
  step (PosSet.add start_pos PosSet.empty) start_pos Up

let count_visited_locations (m : Map.t) =
  let start_pos = find_start m in 
  visited_locations m start_pos |> PosSet.cardinal

(** Determine if the guard will hit a cycle in the map. *)
let has_cycle (m : Map.t) (start : PosDir.t) : bool =
  let rec step visited vec =
    let pos, dir = vec in
    let next_pos = Pos.add pos (Direction.offset dir) in
    match Map.at m next_pos with
    | None ->
      (* We've left the map, we're done and there's no cycle. *)
      false
    | Some '#' ->
      (* We're going to hit an obstacle, turn right. *)
      let new_vec = PosDir.rotate_cw vec in
      step visited new_vec
    | Some _ ->
      (* Must be an open spot ('.') or the starting location ('^'). *)
      let new_vec = PosDir.advance vec in
      if PosDirSet.mem new_vec visited then
        true
      else
        step (PosDirSet.add new_vec visited) new_vec
  in
  step (PosDirSet.singleton start) start
                                     
(** Try placing an obstacle at the given location and checking for a cycle. If
    There is a cycle return true, else return false. *)
let try_obstacle (m : Map.t) (pos: Pos.t) (obs_loc: Pos.t) : bool =
  (* Maps are mutable. I'm sorry for this. *)
  Map.set m obs_loc '#';
  let cycle = has_cycle m (pos, Up) in
  Map.set m obs_loc '.'; cycle

(** Part 2: Count how many places a single obstacle may be placed to cause a cycle. *)
let count_possible_obstructions m =
  let start_pos = find_start m in
  let visited = PosSet.remove start_pos (visited_locations m start_pos) in
  PosSet.filter (fun loc -> try_obstacle m start_pos loc) visited |> PosSet.cardinal
    
let run () =
  let m = read_map "./input/06.txt" in 
  Printf.printf "Day 6: Guard Gallivant\n";
  Printf.printf "locations visited = %d\n" (count_visited_locations m);
  Printf.printf "count possible obstructions = %d\n" (count_possible_obstructions m)
