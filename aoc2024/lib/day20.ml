(* Day 20: Race Condition *)
open Utils
    
type track = {board: char Board.t; start: Coord.t; finish: Coord.t}

let of_string s =
  let board = Board.of_string s in
  let start = Board.find_first board 'S' in
  let finish = Board.find_first board 'E' in
  {board; start; finish}

let mark_path {board; start; finish} =
  let rec aux pos distance path =
    Hashtbl.add path pos distance;
    if pos = finish then path
    else
      let next_cell =
        Coord.neighbors pos
        |> List.filter (fun pos -> Board.at board pos <> '#' && not (Hashtbl.mem path pos))
        |> List.hd
      in
      aux next_cell (distance + 1) path
  in
  let distances = Hashtbl.create 1000 in
  aux start 0 distances

let manhattan (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let cheats_in_radius path start radius =
  let start_dist = Hashtbl.find path start in
  Hashtbl.fold
    (fun finish finish_dist acc ->
       if manhattan start finish <= radius && finish_dist - start_dist > 0 then
         finish :: acc
       else
         acc)
    path
    []

let find_cheats track radius min_cheat =
  let path = mark_path track in
  let cheats = Hashtbl.create (Hashtbl.length path) in
  Hashtbl.iter
    (fun pos dist ->
       List.iter
         (fun shortcut ->
            let d = Hashtbl.find path shortcut - dist - manhattan pos shortcut in
            if d >= min_cheat then 
              match Hashtbl.find_opt cheats d with
              | Some count -> Hashtbl.replace cheats d (count + 1)
              | None -> Hashtbl.add cheats d 1)
         (cheats_in_radius path pos radius))
    path;
  cheats

let count_cheats track radius min_cheat =
  let cheats = find_cheats track radius min_cheat in 
  Hashtbl.fold
    (fun d count acc ->
       if d >= min_cheat then acc + count
       else acc)
    cheats
    0

let run () =                   
  let track = Io.read_file "./input/20.txt" |> of_string in
  Printf.printf "Day 20: Race Condition\n";
  Printf.printf "cheats (radius = 2) = %d\n" (count_cheats track 2 100);
  Printf.printf "cheats (radius = 20) = %d\n" (count_cheats track 20 100);
