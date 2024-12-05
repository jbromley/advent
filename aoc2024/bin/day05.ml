(* Day 5: Print Queue *)

(*******************************)
(* Graph modules and functions *)
(*******************************)

(** Alias for an adjacency list graph. *)
module Graph = Map.Make(Int)
module IntSet = Set.Make(Int)

(** Add an edge to a graph. *)
let add_edge graph src dst =
  let neighbors = match Graph.find_opt src graph with
    | Some lst -> dst :: lst
    | None -> [dst]
  in
  Graph.add src neighbors graph

(** Get the neighbors of a node in a graph. *)
let neighbors g node =
  Graph.find_opt node g

(** Print the edges in a graph. *)
let print_edges g =
  List.iter (fun (src, dst) ->
      let dst_nodes = List.map string_of_int dst in
      Printf.printf "%u -> [%s]\n" src (String.concat "; " dst_nodes)
  ) (Graph.to_seq g |> List.of_seq)

(** Run depth-first search to see if node dst follows node src in the graph.
    If there is a path from src to dst, them return Some(path) where path
    is a list of nodes in the path. If there is no path, return None. *)
let dfs graph src dst =
  let rec explore visited path current =
    if current = dst then
      Some (List.rev (current :: path)) (* Path found, reverse it to return in correct order *)
    else if IntSet.mem current visited then
      None (* Already visited, no path here *)
    else
      let visited = IntSet.add current visited in
      let neighbors = Graph.find_opt current graph |> Option.value ~default:[] in
      let rec try_neighbors = function
        | [] -> None (* No neighbors lead to the destination *)
        | neighbor :: rest -> (
            match explore visited (current :: path) neighbor with
            | Some path -> Some path (* Path found via this neighbor *)
            | None -> try_neighbors rest (* Try the next neighbor *)
          )
      in
      try_neighbors neighbors
  in
  explore IntSet.empty [] src

(*****************************)
(* Input reading and parsing *)
(*****************************)

(** Read the input text for rules and updates. *)
let read_input name : string list * string list =
  let ic = open_in name in 
  let try_read () = try Some(input_line ic) with End_of_file -> None in
  let rec loop rules updates do_rules =
    match try_read () with
    | Some s ->
      if s = "" then loop rules updates false
      else if do_rules then loop (s :: rules) updates true
      else loop rules (s :: updates) false
    | None ->
      close_in ic; (rules, updates) in
  loop [] [] true

(** Parse the rules graph text to build a graph. *)
let parse_graph input =
  let rec add_node edges g =
    match edges with
    | [] -> g
    | e :: es ->
      let new_g = Scanf.sscanf e "%u|%u" (fun src dst -> add_edge g src dst) in
      add_node es new_g
  in
  add_node input Graph.empty

let parse_updates input =
  let rec parse_one lines updates =
    match lines with
    | [] -> updates
    | l :: ls ->
      let update = String.split_on_char ',' l |> (List.map int_of_string) |> Array.of_list in
      parse_one ls (update :: updates)
  in
  parse_one input []


(********************)
(* Puzzle solutions *)
(********************)

(** Check if an update is valid. *)
let is_update_valid update rules =
  let update_list = Array.to_list update in
  let rec check_node nodes src =
    match nodes with
    | [] -> true
    | dst :: tail_nodes ->
      match dfs rules src dst with
      | Some _ -> check_node tail_nodes dst
      | None -> false
  in
  check_node (List.tl update_list) (List.hd update_list)

(** Part 1: Check which updates are valid and sum the middle pages of the
    valid updates. *)
let sum_valid_updates updates rules =
  let rec check_update update sum =
    match update with
    | [] -> sum
    | u :: us ->
      if is_update_valid u rules then
        check_update us (sum + u.(Array.length u / 2))
      else
        check_update us sum
  in
  check_update updates 0
    
  
let run () =
  let rules_text, updates_text = read_input "./input/05.txt" in
  let rules = parse_graph rules_text in
  let updates = parse_updates updates_text in 
  Printf.printf "Day 5: Print Queue\n";
  Printf.printf "safe page sum = %d\n" (sum_valid_updates updates rules);
  (* Printf.printf "count \"MAS\" crosses = %d\n" (count_all_mas_crosses input) *)
