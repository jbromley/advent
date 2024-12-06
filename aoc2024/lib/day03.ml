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
    let rec multiply mult_sum start_pos =
    try
      let _ = Str.search_forward r str start_pos in
      let num1 = int_of_string(Str.matched_group 1 str) in
      let num2 = int_of_string(Str.matched_group 2 str) in
      multiply (mult_sum + num1 * num2) (Str.match_end ())
    with Not_found -> mult_sum
  in
  multiply 0 0

let conditional_multiply_add str =
  let r = Str.regexp "don't()\\|do()\\|mul(\\([0-9][0-9]?[0-9]?\\),\\([0-9][0-9]?[0-9]?\\))" in
  let rec multiply mult_sum enabled start_pos =
    try
      let _ = Str.search_forward r str start_pos in
      match Str.matched_string str with
      | "don't()" -> multiply mult_sum false (Str.match_end ())
      | "do()" -> multiply mult_sum true (Str.match_end ())
      | _ when enabled ->
        let num1 = int_of_string(Str.matched_group 1 str) in
        let num2 = int_of_string(Str.matched_group 2 str) in
        multiply (mult_sum + num1 * num2) enabled (Str.match_end ())
      | _ ->
        multiply mult_sum enabled (Str.match_end ())
    with Not_found -> mult_sum
  in
  multiply 0 true 0
    
let run () =
  let input = read_string "./input/03.txt" in 
  Printf.printf "Day 3: Mull It Over\n";
  Printf.printf "simple multiply = %d\n" (simple_multiply_add input);
  Printf.printf "conditional multiply = %d\n" (conditional_multiply_add input)
