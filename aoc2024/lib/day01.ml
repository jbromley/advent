(* Day 1 *)

(** Read a file with two columns and return a tuple of lists. *)
let read_lists name : (int list * int list) =
  let sc = Scanf.Scanning.open_in name in 
  let try_read () =
    try
      Some (Scanf.bscanf sc "%u %u\n" (fun x1 x2 -> (x1, x2)))
    with End_of_file -> None in
  let rec loop l1 l2 = match try_read () with
    | Some (x1, x2) -> loop (x1 :: l1) (x2 :: l2)
    | None -> Scanf.Scanning.close_in sc; (l1, l2) in
  loop [] []

(** List sorting function using standard compare. *)
let sort = (List.sort compare)
           
(** Part 1: Calculate the distance between two lists. *)
let list_dist l1 l2 =
  let xs = sort l1 in
  let ys = sort l2 in
  List.fold_left (+) 0 (List.map2 (fun x y -> abs(x - y)) xs ys)

(** Given a list, return a hash table where the keys are the integers in the
    list and the values are the number of times that integer appears in the
    list. *)
let frequencies l =
  let freqs = Hashtbl.create (List.length l) in
  let update_freqs x =
    if Hashtbl.mem freqs x then
      let current_freq = Hashtbl.find freqs x in
      Hashtbl.replace freqs x (succ current_freq)
    else
      Hashtbl.replace freqs x 1
  in
  List.iter update_freqs l; freqs

(** Part 2: Calculate the similarity of two lists. *)
let similarity l1 l2 =
  let freqs = frequencies l2 in
  let occurrences x =
    if Hashtbl.mem freqs x then
      Hashtbl.find freqs x
    else
      0
  in
  List.fold_left (+) 0 (List.map (fun x -> x * occurrences x) l1)

(** Execute both parts of the solution. *)
let run () =
  let l1, l2 = read_lists "./input/01.txt" in
  Printf.printf "Day 1: Historian Hysteria\n";
  Printf.printf "distance = %d\n" (list_dist l1 l2);
  Printf.printf "similarity = %d\n" (similarity l1 l2)

