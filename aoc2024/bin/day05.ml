(* Day 5: Print Queue *)

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

(** Alias for an adjacency list graph. *)
module Graph = Map.Make(Int)

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
  let rec add_update lines updates =
    match lines with
    | [] -> updates
    | l :: ls ->
      let update = String.split_on_char ',' l |> (List.map int_of_string) in
      add_update ls (update :: updates)
  in
  add_update input []
  
let run () =
  let rules_text, updates_text = read_input "./input/05.txt" in 
  Printf.printf "Day 4: Print Queue\n";
  (* Printf.printf "count \"XMAS\" = %d\n" (count_all_xmas input);
     Printf.printf "count \"MAS\" crosses = %d\n" (count_all_mas_crosses input) *)
