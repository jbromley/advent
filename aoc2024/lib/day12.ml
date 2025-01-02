(* Day 11: Plutonian Pebbles *)
open Utils
    
let generate_neighbors map cell group =
  List.partition
    (fun c -> Board.at map c = group)
    (Board.neighbors map cell)
  
let explore_region map ((x, y) as pos) visited =
  let this_group = Board.at map pos in
  let rec loop q discovered area perimeter outside_nodes =
    match Queue.take_opt q with 
    | None -> (area * perimeter, Coord.Set.to_list outside_nodes)
    | Some (x, y) ->
      (* Take all neighbors and seperate into on map and in group and other nodes. *)
      let neighbors, others = generate_neighbors map (x, y) this_group in
      (* Filter out nodes that have already been discovered. *)
      let next_nodes = List.filter (fun (nx, ny) -> not (Coord.Set.mem (nx, ny) discovered)) neighbors in
      List.iter (fun (nx, ny) -> Queue.push (nx, ny) q) next_nodes;
      visited.(y).(x) <- true;
      let discovered' = List.fold_left (fun acc node -> Coord.Set.add node acc) discovered next_nodes in
      let area' = area + 1 in
      let perimeter' = perimeter + (4 - List.length neighbors) in 
      loop q discovered' area' perimeter' (Coord.Set.add_seq (List.to_seq others) outside_nodes)
  in 
  let q = Queue.create () in
  Queue.push (x, y) q;
  loop q (Coord.Set.singleton (x, y)) 0 0 Coord.Set.empty

let calculate_fence_cost map =
  let rec loop q visited total_cost =
    match Queue.take_opt q with
    | None -> total_cost
    | Some (x, y) ->
      if visited.(y).(x) then loop q visited total_cost
      else
        let cost, others = explore_region map (x, y) visited in
        List.iter
          (fun (nx, ny) -> if not visited.(ny).(nx) then Queue.push (nx, ny) q)
          others;
        loop q visited (total_cost + cost)
  in
  let width, height = Board.size map in
  let visited = Array.make_matrix height width false in 
  let q = Queue.create () in Queue.push (0, 0) q;
  loop q visited 0

let run () =                   
  let map = Io.read_file "./input/12.txt" |> Board.of_string in
  Printf.printf "Day 12: Garden Groups\n";
  Printf.printf "calculate fence cost = %d\n" (calculate_fence_cost map) ;
  (* Printf.printf "count stones (part 2) = %d\n" (count_stones input 75) *)
