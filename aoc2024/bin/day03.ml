(* Day 2: Red-Nosed Reports *)
    
(** Read a file and return a list of lists of integers. *)
let read_string name : string =
  let ic = open_in name in 
  let try_read () =
    try
      Some (input_line ic)
    with End_of_file -> None in
  let rec loop input =
    match try_read () with
    | Some s ->
      loop (input ^ s)
    | None ->
      close_in ic; input in
  loop ""

(** Part 1: Extract all pairs of multiplicands from valid mul instructions,
    multiply them, and then add the results of the multiplications. *)
let simple_multiply_add str =
    let r = Str.regexp "mul(\\([0-9][0-9]?[0-9]?\\),\\([0-9][0-9]?[0-9]?\\))" in 
    let rec multiply acc start_pos =
    try
      let _ = Str.search_forward r str start_pos in
      let num1 = Str.matched_group 1 str in
      let num2 = Str.matched_group 2 str in
      multiply (int_of_string num1 * int_of_string num2 :: acc) (Str.match_end ())
    with Not_found -> List.fold_left (+) 0 acc
  in
  multiply [] 0

let conditional_multiply_add _str =
  -1
    
let run () =
  let input = read_string "./input/3.txt" in 
  Printf.printf "Day 3: Mull It Over\n";
  Printf.printf "simple multiply = %d\n" (simple_multiply_add input);
  Printf.printf "conditional multiply = %d\n" (conditional_multiply_add input)
