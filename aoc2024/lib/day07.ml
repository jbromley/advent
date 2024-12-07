(* Day 7: Bridge Repair *)

(** The [equation] record describes an input line. The fields are [result],
    which contains the desired result, and [numbers] which are the numbers in
    the equation. *)
type equation = { result: int; numbers: int list }

(** Parses a line of the form {i 3267: 81 40 27} into an [equation] record.
    The number on the left of the colon is the [result] field and the list
    of numbers on the right of the colon are the [numbers] field. *)
let parse_equation line =
  let colon_index = String.index line ':' in
  let result = int_of_string (String.sub line 0 colon_index) in
  let numbers = String.sub line (colon_index + 2) (String.length line - colon_index -2) |>
                String.split_on_char ' ' |> List.map int_of_string in 
  { result; numbers }

(** Reads the file with name [filename], applies the function {f: string -> 'a} to each
    line and returns a list of {'a}. *)
let read_input filename f =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (f line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

(** In this context, the [(||)] operator concatenates two numbers. For example,
    [12 || 34] produces the number 1234. *)
let (||)  = fun x y -> string_of_int x ^ string_of_int y |> int_of_string
                       
(** Generate a list of all combinations of operators in the list [op_list]
    of length [n]. Each operator must have the signature [int -> int -> int]. *)
let rec generate_ops n (op_list: (int -> int -> int) list)  =
  if n = 0 then
    [[]]  (* Base case: there's only one combination of length 0, the empty list *)
  else
    let smaller_combos = generate_ops (n - 1) op_list in
    List.concat (List.map (fun op -> List.map (fun ops -> op :: ops) smaller_combos) op_list)

(** Function to combine a list of numbers and operators and evaluates the
    resulting expression from left to right. *)
let rec eval nums ops =
  match nums, ops with
  | n :: ns, op :: ops ->
      (* Apply the first operator to the first two numbers,
         then recursively evaluate the remaining expression *)
      let result = op n (List.hd ns) in
      eval (result :: List.tl ns) ops
  | [n], [] -> n  (* Base case: when there's one number left and no operators *)
  | _, _ -> failwith "eval: mismatched numbers and operators"

(** Take an [equation] record and attempt to insert [(+)] and [( * )] operators
    between the numbers to get the result. If any combination of operators produces
    a true equation, then return [Some result], otherwise return [None]. *)
let valid_equation_result eq (op_list: (int -> int -> int) list) =
  let { result; numbers } = eq in
  let ops = generate_ops (List.length numbers - 1) op_list in
  if List.exists (fun ops -> eval numbers ops = result) ops then
    Some result
  else
    None

(** Given [eqs], a list of [equation] records, check if each one can be made true by
    inserting the correct operators out of [(+)] and [( * )]. If the equation can be
    made true, add the result to a running sum. Return the sum of the results of all
    valid equations. *)
let sum_2_op_equations eqs =
  let op_list = [(+); ( * )] in 
  List.filter_map (fun eq -> valid_equation_result eq op_list) eqs |> (List.fold_left (+) 0)

(** Given [eqs], a list of [equation] records, check if each one can be made
    true by inserting the correct operators out of [(+)], [( * )], and [(||)].
    If the equation can be made true, add the result to a running sum. Return
    the sum of the results of all valid equations. *)
let sum_3_op_equations eqs =
  let op_list = [(+); ( * ); (||)] in 
  List.filter_map (fun eq -> valid_equation_result eq op_list) eqs |> (List.fold_left (+) 0)

let run () =
  let eqs = read_input "./input/07.txt" parse_equation in 
  Printf.printf "Day 7: Bridge Repair\n";
  Printf.printf "sum of valid two-op equations = %d\n" (sum_2_op_equations eqs);
  Printf.printf "sum of valid three-op equations = %d\n" (sum_3_op_equations eqs)
