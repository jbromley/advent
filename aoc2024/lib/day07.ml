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

let concat_num x y = string_of_int x ^ string_of_int y |> int_of_string
              
let rec is_valid target result nums three_ops =
  if result > target then
    false
  else
    match nums with
    | [] -> result = target
    | n :: ns ->
      is_valid target (result + n) ns three_ops ||
      is_valid target (result * n) ns three_ops ||
      (three_ops && is_valid target (concat_num result n) ns three_ops)

(** Given [eqs], a list of [equation] records, check if each one can be made true by
    inserting the correct operators out of [(+)] and [( * )]. If the equation can be
    made true, add the result to a running sum. Return the sum of the results of all
    valid equations. *)
let sum_equations eqs three_ops =
  List.filter (fun eq -> let { result = target; numbers = nums } = eq in
                is_valid target (List.hd nums) (List.tl nums) three_ops) eqs |>
  (List.fold_left (fun acc eq -> acc + eq.result) 0)

let run () =
  let eqs = read_input "./input/07.txt" parse_equation in 
  Printf.printf "Day 7: Bridge Repair\n";
  Printf.printf "sum of valid two-op equations = %d\n" (sum_equations eqs false);
  Printf.printf "sum of valid three-op equations = %d\n" (sum_equations eqs true)
