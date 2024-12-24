(* Day 18: RAM Run *)
open Utils

let of_string s =
  String.trim s
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ',')
  |> List.map (fun l -> (int_of_string (List.nth l 0), int_of_string (List.nth l 1)))

let on_map (x, y) (max_x, max_y) =
  0 <= x && x <= max_x && 0 <= y && y <= max_y
                                    
let bfs corrupted finish =
  let rec aux visited frontier steps =
    if Coord.Set.mem finish frontier then steps
    else
      let neighbors =
        Coord.Set.to_list frontier
        |> List.map Coord.neighbors
        |> List.fold_left (fun l acc -> acc @ l) []
        |> Coord.Set.of_list
        |> Coord.Set.filter (fun pos -> on_map pos finish && not (Coord.Set.mem pos corrupted))
      in
      aux (Coord.Set.union neighbors visited) neighbors (steps + 1)
  in
  aux (Coord.Set.add (0, 0) corrupted) (Coord.Set.singleton (0, 0)) 0

let rec take n l =
  if n = 0 then []
  else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
                   
let minimum_steps drops finish num_drops =
  let corrupted = take num_drops drops |> Coord.Set.of_list in
  bfs corrupted finish

let run () =                   
  let drops = Io.read_file "./input/18.txt" |> of_string in
  Printf.printf "Day 18: RAM Run\n";
  Printf.printf "minimum steps (1024 ns) = %d\n" (minimum_steps drops (70, 70) 1024)
  (* Printf.printf "trail rating = %d\n" (sum_trailhead_scores map ~rating:false ()) *)
