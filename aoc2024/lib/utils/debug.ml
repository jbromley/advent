let coord_set_to_string s =
  let coords =
    List.map
      (fun elt -> Printf.sprintf "%s" (Coord.to_string elt))
      (Coord.Set.to_list s)
  in
  Printf.sprintf "{ %s }" (String.concat "; " coords)

let int_list_to_string l =
  let ls = List.map string_of_int l in
  "[" ^ String.concat "; " ls ^ "]"

let int_int_list_to_string l =
  let ls = List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) l in
  "[" ^ String.concat "; " ls ^ "]"
