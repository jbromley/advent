(* Day 16: Reindeer Maze *)
open Utils

module Node = struct
  type t = Coord.t * Coord.t * int
  let compare (_, _, c1) (_, _, c2) = compare c1 c2
  let equal (c11, c12, score1) (c21, c22, score2) =
    c11 = c21 && c12 = c22 && score1 = score2
  let hash ((x1, y1), (x2, y2), score) =
    Hashtbl.hash x1 +
    3 * Hashtbl.hash y1 +
    5 * Hashtbl.hash x2 +
    7 * Hashtbl.hash y2 +
    11 * Hashtbl.hash score
end

module Coord2 = struct
  type t = Coord.t * Coord.t
  let compare (p11, p12) (p21, p22) =
    match compare p11 p21 with
    | 0 -> compare p12 p22
    | c -> c
  let equal (p11, p12) (p21, p22) =
    p11 = p21 && p12 = p22
  let hash ((x1, y1), (x2, y2)) =
    Hashtbl.hash x1 +
    3 * Hashtbl.hash y1 +
    5 * Hashtbl.hash x2 +
    7 * Hashtbl.hash y2
end

module Pq = Binary_heap.Make(Node)

module Coord2Hashtbl = Hashtbl.Make(Coord2)

let dijkstra maze starts ?(ends = Coord.Set.empty) () =
  let visited = Coord2Hashtbl.create 1000 in
  let pq = Pq.create ~dummy:((-1, -1), (-1, -1), max_int) 100 in
  List.iter (fun (pos, dir) -> Pq.add pq (pos, dir, 0)) starts;
  let rec aux pq =
    if Pq.is_empty pq then ()
    else
      let (pos, dir, score) = Pq.pop_minimum pq in
      if Coord2Hashtbl.mem visited (pos, dir) then aux pq
      else (
        Coord2Hashtbl.add visited (pos, dir) score;
        if Coord.Set.mem pos ends then ()
        else
          let next_positions =
            let ldir, rdir = Coord.(rotate_ccw dir, rotate_cw dir) in
            [
              (Coord.add pos dir, dir, score + 1);
              (pos, ldir, score + 1000);
              (pos, rdir, score + 1000);
            ]
          in
          let pq =
            List.fold_left
              (fun pq (pos, dir, score) ->
                 match Board.at maze pos with
                 | '#' -> pq
                 | _ ->
                   if Coord2Hashtbl.mem visited (pos, dir) then pq
                   else (Pq.add pq (pos, dir, score); pq))
              pq
              next_positions
          in
          aux pq)
  in
  aux pq;
  visited
    
let find_lowest_score_path maze =
  let start_pos = Board.find_first maze 'S' in
  let end_pos = Board.find_first maze 'E' in
  let distances = dijkstra maze [(start_pos, (1, 0))] ~ends:(Coord.Set.singleton end_pos) () in
  List.find_map
    (fun dir -> Coord2Hashtbl.find_opt distances (end_pos, dir))
    Coord.offsets
  |> Option.value ~default:max_int

let count_path_tiles maze =
  let start_pos = Board.find_first maze 'S' in
  let end_pos = Board.find_first maze 'E' in
  let distances_start = dijkstra maze [(start_pos, (1, 0))] () in
  let distances_end = dijkstra maze (List.map (fun dir -> (end_pos, dir)) Coord.offsets) () in
  let best_score = Coord2Hashtbl.find distances_end (start_pos, (-1, 0)) in
  Board.counti
    maze
    (fun pos ch ->
       if ch = '#' then false
       else
         Coord.Set.exists
           (fun dir ->
              let n1 = Coord2Hashtbl.find_opt distances_start (pos, dir) |> Option.value ~default:(-1) in
              let n2 = Coord2Hashtbl.find_opt distances_end (pos, (Coord.rotate_180 dir)) |> Option.value ~default:(-1) in
           n1 + n2 = best_score)
           (Coord.offsets |> Coord.Set.of_list)) 
    
let run () =
  let maze = Io.read_file "./input/16.txt" |> Board.of_string in
  Printf.printf "Day 16: Reindeer Maze\n";
  Printf.printf "lowest score (part 1) = %d\n" (find_lowest_score_path maze);
  Printf.printf "tiles on best path = %d\n" (count_path_tiles maze)
