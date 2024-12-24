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
    if Coord.Set.cardinal frontier = 0 then -1
    else if Coord.Set.mem finish frontier then steps
    else
      let neighbors =
        Coord.Set.to_list frontier
        |> List.map Coord.neighbors
        |> List.fold_left (fun l acc -> acc @ l) []
        |> Coord.Set.of_list
        |> Coord.Set.filter (fun pos -> on_map pos finish && not (Coord.Set.mem pos visited))
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

let exists_path drops steps finish =
  let corrupted = take steps drops |> Coord.Set.of_list in 
  bfs corrupted finish <> -1

let find_path_blocker drops finish =
  let rec binary_search lo hi =
    let mid = (lo + hi) / 2 in
    if exists_path drops mid finish then
      if not (exists_path drops (mid + 1) finish) then mid
      else binary_search (mid + 1) hi
    else
      binary_search lo (mid - 1)
  in
  let index = binary_search 0 (List.length drops) in
  List.nth drops index

let run () =                   
  let drops = Io.read_file "./input/18.txt" |> of_string in
  Printf.printf "Day 18: RAM Run\n";
  Printf.printf "minimum steps (1024 ns) = %d\n" (minimum_steps drops (70, 70) 1024);
  let x, y = find_path_blocker drops (70, 70) in
  Printf.printf "path blocking drop = (%d, %d)\n" x y;
