let of_2_list (lst : 'a list) : 'a * 'a =
  match lst with
  | elt1 :: elt2 :: [] -> (elt1, elt2)
  | _ -> failwith "of_2_list: invalid list length"

let of_3_list (lst : 'a list) : 'a * 'a * 'a =
  match lst with
  | elt1 :: elt2 :: elt3 :: [] -> (elt1, elt2, elt3)
  | _ -> failwith "of_3_list: invalid list length"

let of_4_list (lst : 'a list) : 'a * 'a * 'a * 'a =
  match lst with
  | elt1 :: elt2 :: elt3 :: elt4 :: [] -> (elt1, elt2, elt3, elt4)
  | _ -> failwith "of_4_list: invalid list length"
