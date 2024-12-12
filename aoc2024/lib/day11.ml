(* Day 11: Plutonian Pebbles *)

module PairHash = struct
  type t = int * int
  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
  let hash (x, y)  = Hashtbl.hash x + Hashtbl.hash y
end

module PairHashtbl = Hashtbl.Make(PairHash)
    
let read_input name =
  let ic = open_in name in 
  let line = input_line ic in
  close_in ic;
  line

let parse_input line =
  String.split_on_char ' ' line |> List.map int_of_string

let split_number n =
  let s = string_of_int n in
  let l = String.length s in
  if l mod 2 = 0 then
    let i = (l / 2) in
    [String.sub s 0 i |> int_of_string;
     String.sub s i i |> int_of_string]
  else
    failwith "split_number: odd number of digits"

let next_stones n =
  if n = 0 then [1]
  else if String.length (string_of_int n) mod 2 = 0 then split_number n
  else [n * 2024]

let blink stones = List.map next_stones stones |> List.concat
                                                    
let count_stones_slow stones blinks =
  Seq.fold_left (fun acc _ -> blink acc) stones (Seq.init blinks (fun i -> i)) |>
  List.length

let count_stones stones blinks =
  let cache = Hashtbl.create 128 in
  let rec count_for_stone stone blinks =
    if blinks = 0 then 1
    else
      match Hashtbl.find_opt cache (stone, blinks) with
      | Some result -> result
      | None ->
        let result = next_stones stone |> List.fold_left (fun acc n -> acc + count_for_stone n (blinks - 1)) 0 in
        Hashtbl.add cache (stone, blinks) result;
        result
  in
  List.fold_left (fun acc stone -> acc + count_for_stone stone blinks) 0 stones

let run () =                   
let input = read_input "./input/11.txt" |> parse_input in
Printf.printf "Day 11: Plutonian Pebbles\n";
Printf.printf "count stones (part 1) = %d\n" (count_stones_slow input 25) ;
Printf.printf "count stones (part 2) = %d\n" (count_stones input 75)
