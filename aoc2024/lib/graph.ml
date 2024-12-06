(*******************************)
(* Graph modules and functions *)
(*******************************)

(** Alias for an adjacency list graph. *)
module Graph = Map.Make(Int)
module IntSet = Set.Make(Int)

(** Add an edge to a graph. *)
let add_edge graph src dst =
  let neighbors = match Graph.find_opt src graph with
    | Some set -> IntSet.add dst set
    | None -> IntSet.singleton dst
  in
  Graph.add src neighbors graph

(** Get the neighbors of a node in a graph as a set. *)
let neighbors g node =
  Graph.find_opt node g |> Option.value ~default:IntSet.empty

(** Get a list of neighbors for the given node. *)
let neighbor_list g node =
  neighbors g node |> IntSet.elements
  

(** Print the edges in a graph. *)
let print_edges g =
  List.iter (fun (src, dst) ->
      let dst_nodes = List.map string_of_int (IntSet.to_list dst) in
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
      let neighbors = Graph.find_opt current graph |>
                      Option.value ~default:IntSet.empty |>
                      IntSet.elements in
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
