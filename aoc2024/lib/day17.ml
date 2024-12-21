(* Day 17: Chronospatial Computer *)
open Utils

type computer = {
  program: int array;
  ip : int;
  a : int;
  b : int;
  c : int;
  output : int list
}

let of_string s =
  let next_int s si =
    let i = 2 + String.index_from s si ':' in
    let nl = String.index_from s i '\n' in
    let len = nl - i in
    (int_of_string (String.sub s i len), nl + 1)
  in
  let a, n = next_int s 0 in
  let b, n = next_int s n in
  let c, n = next_int s n in
  let pindex = 2 + String.index_from s n ':' in
  let ptext = String.sub s pindex (String.length s - pindex) |> String.trim in
  let program =
    String.split_on_char ',' ptext
    |> List.map (fun s -> int_of_string s)
    |> Array.of_list
  in
  { program; ip = 0; a; b; c; output = [] }

let literal_operand { program; ip; _ } =
  program.(ip + 1)

let combo_operand { program; ip; a; b; c; _ } =
  match program.(ip + 1) with
  | (0 | 1 | 2 | 3) as n -> n
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | op -> failwith ("combo_operand: bad operand " ^ (string_of_int op))

let debug _comp =
  []
  
let run () =
  let _input = Io.read_file "./input/17-test.txt" |> of_string in
  Printf.printf "Day 17: Chronospatial Computer\n";
  (* Printf.printf "lowest score (part 1) = %d\n" (find_lowest_score_path maze); *)
  (* Printf.printf "tiles on best path = %d\n" (count_path_tiles maze) *)
