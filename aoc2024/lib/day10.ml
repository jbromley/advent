(* Day 10: Hoof It *)

module Pos = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | others -> others
end
module PosSet = Set.Make(Pos)
    
let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]

let read_map name =
  let ic = open_in name in 
  let try_read () =
    try
      Some(input_line ic)
    with End_of_file -> None in
  let rec loop input =
    match try_read () with
    | Some s ->
      loop (s :: input)
    | None ->
      close_in ic; List.rev input
  in
  loop []

let parse_map input =
  List.map
    (fun s -> String.to_seq s |> Seq.map (fun ch -> Char.code ch - Char.code '0') |> Array.of_seq)
    input |> Array.of_list
    
let on_map map (r, c) =
  if r >= 0 && r < Array.length map &&
     c >= 0 && c < Array.length map.(0) then Some (r, c)
  else None

let dims map =
  (Array.length map, Array.length map.(0))

let print_step map r c next_nodes =
  Printf.printf "node = (%d, %d)\taltitude = %d\tneigbors = " r c map.(r).(c);
  List.iter (fun (r, c) -> Printf.printf "(%d, %d) " r c) next_nodes;
  print_endline ""
    
let bfs map r c rating =
  let rec loop q discovered trails =
    match Queue.take_opt q with
    | None -> trails
    | Some (r, c) ->
      if map.(r).(c) = 9 then loop q discovered (trails + 1)
      else (
        let next_nodes = List.map (fun (dr, dc) -> (r + dr, c + dc)) directions |>
                         List.filter_map (fun pos -> on_map map pos) |>
                         List.filter (fun (nr, nc) ->
                             map.(nr).(nc) = map.(r).(c) + 1 &&
                             (rating || not (PosSet.mem (nr, nc) discovered))) in
        List.iter (fun (nr, nc) -> Queue.push (nr, nc) q) next_nodes;
        let discovered' = List.fold_left (fun acc node -> PosSet.add node acc) discovered next_nodes in
        loop q discovered' trails)
  in
  let q = Queue.create () in
  Queue.push (r, c) q;
  loop q (PosSet.singleton (r, c)) 0

let sum_trailhead_scores map ?(rating = false) () =
  let total_score = ref 0 in
  Array.iteri
    (fun r row ->
      Array.iteri (fun c elt ->
          if elt = 0 then let score = bfs map r c rating in total_score := !total_score + score) row)
    map;
  !total_score

let run () =                   
  let map = read_map "./input/10.txt" |> parse_map in
  Printf.printf "Day 10: Hoof It\n";
  Printf.printf "trail score = %d\n" (sum_trailhead_scores map ~rating:false ());
  Printf.printf "trail rating = %d\n" (sum_trailhead_scores map ~rating:false ())
