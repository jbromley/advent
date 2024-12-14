let print_bool_matrix m =
  Array.iter
    (fun row ->
       Printf.printf "%s\n" (Array.map (fun elt -> if elt then "X " else "O ") row |> Array.to_seq |> List.of_seq |> (String.concat " ")))
    m
  
let print_map map =
  let sprintf = Printf.printf in
  let _, cols = dims map in
  sprintf " | %s\n" (String.concat " " (List.map string_of_int (List.init 10 (fun i -> i))));
  sprintf " | %s\n" (String.init (2 * cols - 1) (fun _i -> '-'));
  Array.iteri
    (fun i row ->
       sprintf
         "%d| %s\n"
         i
         (Array.map (fun ch -> String.make 1 ch) row |> Array.to_seq |> List.of_seq |> (String.concat " ")))
    map

let print_step group (r, c) next_nodes area perimeter =
  let pair_to_string (r, c) = Printf.sprintf "(%d, %d)" r c in
  let next = String.concat "; " (List.map (fun p -> pair_to_string p) next_nodes) in
  Printf.printf "%c (%d, %d) area = %d perimeter = %d next_nodes = [ %s ]\n" group r c area perimeter next

