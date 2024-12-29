(*  Day 22: Monkey Market *)
open Utils

module Tuple4 = struct
  type t = int * int * int * int
  let to_list t =
    match t with
    | (t1, t2, t3, t4) -> [t1; t2; t3; t4]
  let of_list l =
    match l with
    | t1 :: t2 :: t3 :: t4 :: [] -> (t1, t2, t3, t4)
    | _ -> invalid_arg "Tuple4.of_list: invalid list length"
  let compare t1 t2 =
    compare (to_list t1) (to_list t2)
  module Set = Set.Make
end

module Tuple4Set = Set.Make(Tuple4)

let of_string s =
  String.trim s |> String.split_on_char '\n' |> List.map int_of_string

let next_random seed =
  let s1 = ((seed lsl 6) lxor seed) mod 16777216 in
  let s2 = ((s1 lsr 5) lxor s1) mod 16777216 in
  ((s2 lsl 11) lxor s2) mod 16777216
    
let rec generate_secret iters seed =
  if iters = 0 then seed
  else generate_secret (iters - 1) (next_random seed)

let sum_secrets iters secrets =
  List.fold_left
    (fun sum secret -> sum + generate_secret iters secret)
    0
    secrets

let maximize_bananas iters monkeys =
  let rec simulate_monkey seed iters changes visited sells =
    if iters = 0 then sells
    else
      let seed' = next_random seed in
      let change = seed' mod 10 - seed mod 10 in 
      Queue.add change changes;
      if Queue.length changes = 4 then
        let key = Queue.to_seq changes |> List.of_seq |> Tuple4.of_list in
        if not (Tuple4Set.mem key visited) then (
          match Hashtbl.find_opt sells key with
          | None -> Hashtbl.add sells key [seed' mod 10]
          | Some s -> Hashtbl.add sells key (seed' mod 10 :: s));
        let _ = Queue.pop changes in 
        simulate_monkey seed' (iters - 1) changes (Tuple4Set.add key visited) sells
      else
        simulate_monkey seed' (iters - 1) changes visited sells
  in
  let rec simulate ms iters sells =
    match ms with
    | [] ->
      let bananas_sold = Seq.map (fun bananas -> List.fold_left (+) 0 bananas) (Hashtbl.to_seq_values sells) in
      Seq.fold_left max min_int bananas_sold
    | seed :: other_monkeys ->
      let sells' = simulate_monkey seed iters (Queue.create ()) (Tuple4Set.empty) sells in 
      simulate other_monkeys iters sells'
  in
  simulate monkeys iters (Hashtbl.create 8000)

let run () =                   
  let secrets = Io.read_file "./input/22.txt" |> of_string in
  Printf.printf "Day 22: Monkey Market\n";
  Printf.printf "sum of secrets = %d\n" (sum_secrets 2000 secrets);
  Printf.printf "maximum bananas = %d\n" (maximize_bananas 2000 secrets);

 
