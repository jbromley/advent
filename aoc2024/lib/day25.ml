(*  Day 25: Code Chronicle *)
open Utils

type kind =
  | Key of int array
  | Lock of int array
              
let split_on_double_newline s =
  let r = Str.regexp_string "\n\n" in
  Str.global_replace r "\030" s |> String.split_on_char '\030'

let of_string s =
  let heights lst =
    let x = Array.init 5 (fun _ -> 0) in
    List.iter
      (fun line ->
         List.iteri (fun i elt -> if elt = '#' then x.(i) <- x.(i) + 1) line)
      lst;
    x
  in
  let decode_block lst =
    let blk = List.map (fun s -> String.to_seq s |> List.of_seq) lst in 
    if List.hd (List.hd blk) = '#' then
      Lock (heights (List.tl blk))
    else
      Key (heights (List.tl (List.rev blk)))
  in
  split_on_double_newline (String.trim s)
  |> List.map (String.split_on_char '\n')
  |> List.fold_left
    (fun (keys, locks) blk ->
       match decode_block blk with
       | Key k -> (k :: keys, locks)
       | Lock l -> (keys, l :: locks))
    ([], [])

let cartesian_product lst1 lst2 =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) lst2) lst1)

let key_lock_match key lock =
  Array.for_all2 (fun k l -> k + l <= 5) key lock
    
let count_matches keys locks =
  List.fold_left
    (fun matches (k, l) -> if key_lock_match k l then matches + 1 else matches)
    0
    (cartesian_product keys locks)

let run () =                   
  let keys, locks = Io.read_file "./input/25.txt" |> of_string in
  Printf.printf "Day 25: Code Chronicle\n";
  Printf.printf "key/lock matches = %d\n" (count_matches keys locks);
