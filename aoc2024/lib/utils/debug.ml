  let coord_set_to_string s =
    let coords =
      List.map
        (fun elt -> Printf.sprintf "%s" (Coord.to_string elt))
        (Coord.Set.to_list s)
    in
    Printf.sprintf "{ %s }" (String.concat "; " coords)
