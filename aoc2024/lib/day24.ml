(*  Day 24: Crossed Wires *)
open Utils

module ValueMap = Map.Make(String)

module Gate = struct
  type t = (int -> int -> int) * string * string * string
           
  let compare (_op1, in11, in12, out1) (_op2, in21, in22, out2) =
    compare (in11, in12, out1) (in21, in22, out2)
      
  let ops = [("AND", (land)); ("OR", (lor)); ("XOR", (lxor))]

  let step (op, in1, in2, out) values =
    if ValueMap.mem in1 values && ValueMap.mem in2 values then
      let value = op (ValueMap.find in1 values) (ValueMap.find in2 values) in
      Some (ValueMap.add out value values)
    else
      None
    
  let of_string s =
    let r = Str.regexp {|\([a-z][a-z0-9][a-z0-9]\) \(AND\|OR\|XOR\) \([a-z][a-z0-9][a-z0-9]\) -> \([a-z][a-z0-9][a-z0-9]\)|} in
    let _ = Str.search_forward r s 0 in
    let in1 = Str.matched_group 1 s in
    let op = Str.matched_group 2 s in
    let in2 = Str.matched_group 3 s in
    let out = Str.matched_group 4 s in
    (List.assoc op ops, in1, in2, out)
end

module GateSet = Set.Make(Gate)
 
type circuit = { values: int ValueMap.t; gates: GateSet.t }
               
let split_on_double_newline s =
  let r = Str.regexp_string "\n\n" in
  Str.global_replace r "\030" s |> String.split_on_char '\030'

let of_string (s : string) : circuit =
  let parse_value line =
    let lst = String.split_on_char ':' line in
    (List.hd lst, String.trim (List.nth lst 1) |> int_of_string)
  in
  let vs, gs = split_on_double_newline s |> List.map String.trim |> Tuple.of_2_list in
  let values : int ValueMap.t =
    List.fold_left
      (fun acc s ->
         let reg, value = parse_value s in
         ValueMap.add reg value acc)
      ValueMap.empty
      (String.split_on_char '\n' vs)
  in
  let gates =
    List.fold_left
      (fun acc s -> GateSet.add (Gate.of_string s) acc)
      GateSet.empty
      (String.split_on_char '\n' gs)
  in
  { values; gates }

let run_circuit c =
  let rec step ({ gates; _ } as c) =
    if GateSet.is_empty gates then c
    else
      let c' = 
        GateSet.fold
          (fun gate ({values = vs; gates = gs} as c) ->
             match Gate.step gate vs with
             | None -> c
             | Some vs' ->
               let gs' = GateSet.remove gate gs in
               { values = vs'; gates = gs' })
          gates
          c
      in
      step c'
  in
  step c

let read_circuit ?(prefix = "z") { values; _ } =
  let outputs =
    ValueMap.filter
      (fun gate _value -> String.starts_with ~prefix:prefix gate)
      values
  in
  ValueMap.to_list outputs
  |> List.sort compare
  |> List.mapi (fun i (_, value) -> value lsl i)
  |> List.fold_left (+) 0

let get_output c =
  run_circuit c |> read_circuit ~prefix:"z"

let mismatched_bits x y =
  let rec aux i n mismatches =
    if n = 0 then mismatches
    else
      aux (i + 1) (n lsr 1) (if n land 1 = 0 then mismatches else (i :: mismatches))
  in
  aux 0 (x lxor y) []

let run () =                   
  let circuit = Io.read_file "./input/24.txt" |> of_string in
  Printf.printf "Day 24: Crossed Wires\n";
  Printf.printf "circuit output = %d\n" (get_output circuit);
  (* Printf.printf "password = %s\n" (find_password g); *)

 
