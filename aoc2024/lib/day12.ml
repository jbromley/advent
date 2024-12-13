(* Day 11: Plutonian Pebbles *)

module Pos = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | others -> others
end
module PosSet = Set.Make(Pos)

type graph_colors = White | Gray | Black
                    
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
    (fun s -> String.to_seq s |> Array.of_seq)
    input |> Array.of_list
    
let on_map map (r, c) =
  r >= 0 && r < Array.length map && c >= 0 && c < Array.length map.(0)

let dims map =
  (Array.length map, Array.length map.(0))

let print_step group (r, c) next_nodes area perimeter =
  let pair_to_string (r, c) = Printf.sprintf "(%d, %d)" r c in
  let next = String.concat "; " (List.map (fun p -> pair_to_string p) next_nodes) in
  Printf.printf "%c (%d, %d) area = %d perimeter = %d next_nodes = [ %s ]\n" group r c area perimeter next

let explore_region map (row, col) =
  let this_group = map.(row).(col) in
  let rec loop q discovered area perimeter outside_nodes =
    match Queue.take_opt q with 
    | None -> (area * perimeter, outside_nodes)
    | Some (r, c) ->
      (* Take all neighbors and seperate into on map and in group and other nodes. *)
      let neighbors, others =
        List.partition (fun (nr, nc) -> map.(nr).(nc) = this_group)
          (List.filter_map
             (fun (dr, dc) -> let npos =  (r + dr, c + dc) in
               if on_map map npos then Some npos else None)
             directions) 
      in
      (* Filter out nodes that have already been discovered. *)
      let next_nodes = List.filter (fun (nr, nc) -> not (PosSet.mem (nr, nc) discovered)) neighbors in
      List.iter (fun (nr, nc) -> Queue.push (nr, nc) q) next_nodes;
      let discovered' = List.fold_left (fun acc node -> PosSet.add node acc) discovered next_nodes in
      let area' = area + 1 in
      let perimeter' = perimeter + (4 - List.length neighbors) in 
      (* print_step this_group (r, c) next_nodes next_area next_perimeter; *)
      loop q discovered' area' perimeter' (PosSet.add_seq (List.to_seq others) outside_nodes)
  in 
  let q = Queue.create () in
  Queue.push (row, col) q;
  loop q (PosSet.singleton (row, col)) 0 0 PosSet.empty

(* let calculate_fence_cost map = *)
(*   let rec loop global_q colors total_cost = *)
(*     match Queue.take_opt global_q with *)
(*     | None -> total_cost *)
(*     | Some (r,c) -> *)
(*       if colors.(r).(c) = Black then loop global_q colors total_cost *)
(*       else *)
(*         let region_cost = explore_region map (r, c) global_q colors in *)
(*         loop global_q colors (total_cost + region_cost) *)
(*   in *)
(*   let global_q = Queue.create () in *)
(*   let rows, cols = dims map in *)
(*   let colors = Array.make_matrix rows cols Black in *)
(*   Queue.push (0, 0) global_q; *)
(*   colors.(0).(0) <- Gray; *)
(*   loop global_q colors 0 *)

let run () =                   
  let _map = read_map "./input/12.txt" |> parse_map in
  Printf.printf "Day 12: Garden Groups\n";
  (* Printf.printf "calculate fence cost = %d\n" (calculate_fence_cost map) ; *)
  (* Printf.printf "count stones (part 2) = %d\n" (count_stones input 75) *)
