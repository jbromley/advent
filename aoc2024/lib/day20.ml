(* Day 20: Race Condition *)
open Utils
    
type track = {board: char Grid.t; start: Coord.t; finish: Coord.t}

let of_string s =
  let board = Grid.of_string s in
  let start = Grid.find_first board 'S' in
  let finish = Grid.find_first board 'E' in
  {board; start; finish}

let mark_path {board; start; finish} =
  let rec aux pos last_pos distance path =
    let path' = (pos, distance) :: path in 
    if pos = finish then List.rev path'
    else
      let next_cell =
        Coord.neighbors pos
        |> List.filter (fun pos -> Grid.at board pos <> '#' && pos <> last_pos)
        |> List.hd
      in
      aux next_cell pos (distance + 1) path'
  in
  aux start (-1, -1) 0 []

let manhattan (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let cheats_in_radius (start, start_dist) path radius min_cheat =
  List.fold_left 
    (fun acc (finish, finish_dist) ->
       let mdist = manhattan start finish in 
       if mdist <= radius && finish_dist - start_dist - mdist >= min_cheat then
         acc + 1
       else
         acc)
    0
    path

let count_cheats track radius min_cheat =
  let rec aux path cheats =
    match path with
    | [] -> cheats
    | start :: rest_path ->
      aux rest_path (cheats + cheats_in_radius start rest_path radius min_cheat)
  in
  aux (mark_path track) 0

let run () =                   
  let track = Io.read_file "./input/20.txt" |> of_string in
  Printf.printf "Day 20: Race Condition\n";
  Printf.printf "cheats (radius = 2) = %d\n" (count_cheats track 2 100);
  Printf.printf "cheats (radius = 20) = %d\n" (count_cheats track 20 100);
