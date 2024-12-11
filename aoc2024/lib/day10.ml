(* Day 10: Hoof It *)

type map = int array array

type graph_color = White | Gray | Black

let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]

let read_map name : map =
  let ic = open_in name in 
  let try_read () =
    try
      Some(input_line ic)
    with End_of_file -> None in
  let rec loop input =
    match try_read () with
    | Some s ->
      let row = String.to_seq s |>
                Seq.map (fun ch -> Char.code ch - Char.code '0') |>
                Array.of_seq
      in
      loop (row :: input)
    | None ->
      close_in ic; List.rev input |> Array.of_list
  in
  loop []

let on_map map (r, c) =
  if r >= 0 && r < Array.length map &&
     c >= 0 && c < Array.length map.(0) then Some (r, c)
  else None

let dims map =
  (Array.length map, Array.length map.(0))

let print_step r c next_nodes =
  Printf.printf "node = (%d, %d), neigbors = " r c;
  List.iter (fun (r, c) -> Printf.printf "(%d, %d) " r c) next_nodes;
  print_endline ""
    
let bfs map r c =
  let rec loop q color trails =
    match Queue.take_opt q with
    | None -> trails
    | Some (r, c) ->
      if map.(r).(c) = 9 then (color.(r).(c) <- Black; loop q color (trails + 1))
      else (
        let next_nodes = List.map (fun (dr, dc) -> (r + dr, c + dc)) directions |>
                         List.filter_map (fun pos -> on_map map pos) |>
                         List.filter (fun (nr, nc) -> (color.(nr).(nc) = White) &&
                                                      map.(nr).(nc) = map.(r).(c) + 1) in
        List.iter (fun (nr, nc) -> color.(nr).(nc) <- Gray; Queue.push (nr, nc) q) next_nodes;
        color.(r).(c) <- Black;
        loop q color trails)
  in
  let rows, cols = dims map in
  let q = Queue.create () in
  Queue.push (r, c) q;
  loop q (Array.make_matrix rows cols White) 0

let sum_trailhead_scores map =
  let total_score = ref 0 in
  Array.iteri
    (fun r row ->
      Array.iteri (fun c elt ->
          if elt = 0 then let score = bfs map r c in total_score := !total_score + score) row)
    map;
  !total_score

let run () =                   
  let map = read_map "./input/10.txt" in (*  *)
  Printf.printf "Day 10: Hoof It\n";
  Printf.printf "trail score = %d\n" (sum_trailhead_scores map);
(* Printf.printf "checksum, files defrag = %d\n" (defragment fs ~by_blocks:false () |> checksum) *)

