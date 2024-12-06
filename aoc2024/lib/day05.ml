(* Day 5: Print Queue *)

module IntPair = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | other -> other
end

module IntPairSet = Set.Make(IntPair)
    
(** Read the input file and return a list of lines where each line is a
    string. *)
let read_input name =
  let ic = open_in name in
  let try_read () = try Some(input_line ic) with End_of_file -> None in
  let rec read_line lines =
    match try_read () with
    | Some s -> read_line (s :: lines)
    | None -> close_in ic; List.rev lines
  in
  read_line []

(** Parse the string list input and return a set of rules and a list of
    updates. The rules are represented as a set of integer pairs and the
    updates are just integer lists. *)
let parse_input input =
  let rec get_rule rules = function
    | [] -> raise Parsing.Parse_error
    | "" :: lines -> (rules, lines)
    | line :: lines ->
      let rule = Scanf.sscanf line "%u|%u" (fun n1 n2 -> (n1, n2)) in 
      get_rule (IntPairSet.add rule rules) lines
  in
  let rules, lines = get_rule IntPairSet.empty input in
  let updates = List.map (fun line -> String.split_on_char ',' line |> List.map int_of_string) lines in 
  (rules, updates)
  

(** Check if an update is valid. *)
let is_update_valid rules update =
  let rec get_pairs acc = function
    | [] | _ :: []  -> acc
    | page :: pages -> get_pairs (acc @ List.map (fun n -> (page, n)) pages) pages
  in
  let pairs = get_pairs [] update in
  List.for_all (fun (src, dst) -> (IntPairSet.mem (src, dst) rules)) pairs

(** Get the nth element of a list. *)
let list_nth lst index =
  if index >= 0 && index < List.length lst then
    let rec loop l n =
      match n with
      | 0 -> List.hd l
      | _ -> loop (List.tl l) (pred n)
    in
    loop lst index
  else
    raise (Invalid_argument "list_nth: index out of range")

(** Part 1: Check which updates are valid and sum the middle pages of the
    valid updates. *)
let sum_valid_updates rules updates =
  List.fold_left (+) 0 (List.map
                          (fun update -> if is_update_valid rules update then list_nth update (List.length update / 2) else 0)
                          updates)

(** Set of integer type. *)
module IntSet = Set.Make(Int)
    
(** Fix an update by reordering its pages. *)
let fix_update rules update =
  (* Change the update to a set of numbers. While the set isn't empty, find a
     number that goes first according to the rules. Add this number to the
     result, remove it from the set and iterate. *)
  let rec loop fixed_update update =
    if IntSet.cardinal update = 0 then
      List.rev fixed_update
    else
      let next_set = IntSet.filter (fun n1 -> (IntSet.exists (fun n2 -> IntPairSet.mem (n2, n1) rules) update) |> not) update in
      let next = IntSet.choose next_set in
      loop (next :: fixed_update) (IntSet.remove next update)
  in
  loop [] (IntSet.of_list update)

(** Part 2: Find invalid updates and fix them, then sum the middle pages. *)
let sum_fixed_updates rules updates =
  List.filter (fun update -> not (is_update_valid rules update)) updates |>
  List.map (fun bad_update -> fix_update rules bad_update) |>
  List.fold_left (fun sum update -> sum + list_nth update (List.length update / 2)) 0
    
let run () =
  let input = read_input "./input/05.txt" in
  let rules, updates = parse_input input in
  Printf.printf "Day 5: Print Queue\n";
  Printf.printf "valid updates middle page sum = %d\n" (sum_valid_updates rules updates);
  Printf.printf "fixed updates middle page sum = %d\n" (sum_fixed_updates rules updates)
