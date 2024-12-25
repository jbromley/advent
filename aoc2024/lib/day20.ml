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

let cheats_in_radius {board; _} path pos r =
  List.filter_map
    (fun offset ->
       let neighbor = Coord.add pos offset in
       let target = Coord.(add pos (mul r offset)) in
       if Board.at board neighbor = '#' && Hashtbl.mem path target then
         Some target
       else
         None)
    Coord.offsets

let count_cheats track =
  let path = mark_path track in
  let cheats = Hashtbl.create (Hashtbl.length path) in
  Hashtbl.iter
    (fun pos dpos ->
       List.iter
         (fun shortcut ->
            let d = Hashtbl.find path shortcut - dpos - 2 in
            if d > 0 then 
              match Hashtbl.find_opt cheats d with
              | Some count -> Hashtbl.replace cheats d (count + 1)
              | None -> Hashtbl.add cheats d 1)
         (cheats_in_radius track path pos 2))
    path;
  cheats

let count_long_cheats distance cheats =
  Hashtbl.fold
    (fun d count acc ->
       if d >= distance then acc + count
       else acc)
    cheats
    0

let run () =                   
  let track = Io.read_file "./input/20.txt" |> of_string in
  Printf.printf "Day 20: Race Condition\n";
  Printf.printf "long cheats = %d\n" (count_cheats track |> count_long_cheats 100);
  (* Printf.printf "different arrangements = %d\n" (count_arrangements patterns designs) *)
