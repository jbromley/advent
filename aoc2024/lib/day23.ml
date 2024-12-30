(*  Day 23: LAN Party *)
open Utils

module G = Map.Make(String)

module VertexSet = Set.Make(struct
  type t = string list
  let compare = compare
end)

let of_string s =
  String.trim s
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char '-')
  |> List.fold_left
    (fun g vertices ->
       match vertices with
       | v1 :: v2 :: [] ->
         G.(add_to_list v1 v2 g |> add_to_list v2 v1)
       | _ -> failwith "of_string: invalid edge format")
    G.empty

let find_combinations lst f =
  let rec search_combos lst found =
    match lst with
    | [] -> found
    | x :: xs ->
      match List.filter (fun y -> f x y) xs with
      | [] -> search_combos xs found
      | vs ->
        search_combos xs (List.map (fun v -> [x; v]) vs @ found)
  in
  search_combos lst []

let have_edge g v1 v2 =
  List.mem v2 (G.find v1 g)

let find_3_cliques g =
  G.fold
    (fun v neighbors cliques -> 
       match find_combinations neighbors (have_edge g) with
       | [] -> cliques
       | edges ->
         List.fold_left
           (fun acc edge ->
              let clique = List.sort compare (v :: edge) in 
              if List.exists (String.starts_with ~prefix:"t") clique then
                VertexSet.add clique acc
              else
                acc)
           cliques
           edges)
    g
    VertexSet.empty

let count_3_cliques g =
  find_3_cliques g |> VertexSet.cardinal

let all_connected g vertices =
  match vertices with
  | u :: vs ->
    List.for_all (fun v -> List.mem v (G.find u g)) vs
  | _ -> false

let rec combinations lst r =
  if r = 0 then [[]]
  else match lst with
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun comb -> x :: comb) (combinations xs (r - 1)) in
        let without_x = combinations xs r in
        with_x @ without_x

let maximal_connected_subgraph g =
  let longer_list lst1 lst2 =
    if List.length lst1 >= List.length lst2 then lst1 else lst2
  in
  let rec aux vertices best =
    match vertices with
    | [] -> List.sort compare best
    | (v, neighbors) :: vs ->
      let len_neighbors = List.length neighbors in
      for i = len_neighbors downto 2 do
        if i < List.length best then aux vs best
        else
          let combos = combinations neighbors i in
          let best' = 
            List.fold_left
              (fun acc combo ->
                 if all_connected g combo then
                   let group = v :: combo in
                   longer_list group best
                 else
                   acc)
              best
              combos
          in
          aux vs best'
      done
  in
  aux (G.to_list g) []
    


      
  
                        
let run () =                   
  let g = Io.read_file "./input/23.txt" |> of_string in
  Printf.printf "Day 23: LAN Party\n";
  Printf.printf "number of 3-cliques with ts = %d\n" (count_3_cliques g);
  (* Printf.printf "maximum bananas = %d\n" (maximize_bananas 2000 secrets); *)

 