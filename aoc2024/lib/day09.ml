(* Day 9: Disk Fragmenter *)

module IntMap = Map.Make(Char)

(** Take the [int list] and build the file map. The file map is a list of blocks.
    of form [(int * int) array] containing the ID and the size. A block is [(int * int)]
    of the form (id, size). If a block is free, it's ID is [-1]. *)
let build_block_map lst =
  let make_blocks id size = List.init size (fun _ -> (id, 1)) in
  let rec loop id acc l =
    match l with
    | [] -> acc
    | [blocks] -> make_blocks id blocks @ acc
    | file :: free :: others ->
      let new_acc = make_blocks (-1) free @ make_blocks id file @ acc in
      loop (id + 1) new_acc others
  in
  let block_list = loop 0 [] lst in
  block_list |> List.rev |> List.to_seq |> Array.of_seq

(** Read the input file with name [filename] and return a the string as
    a [char list]. *)
let read_input filename =
  let ic = open_in filename in
  let line = input_line ic in
  close_in ic;
  String.to_seq line |> List.of_seq |> List.map (fun ch -> Char.code ch - Char.code '0') |> build_block_map

(** Find the first element of the array that satisfies the predicate and return
    the index. *)
let find_index pred arr =
  let len = Array.length arr in
  let rec loop i =
    if i >= len then None
    else if pred arr.(i) then Some i
    else loop (i + 1)
  in
  loop 0
    
(** Take a block array [(int * int) array]) with the id and size of blocks and
    defragment it. *)
let defragment block_map =
  let rec loop ilo i =
    match i with
    | 0 -> block_map
    | i ->
      let this_id, this_size = block_map.(i) in
      if this_id = -1 then loop ilo (pred i)
      else 
        let free_block = find_index (fun (id, size) ->id = (-1) && this_size <= size) (Array.sub block_map ilo (i - ilo)) in
        match free_block with
        | Some(j) ->
          Array.set block_map (ilo + j) (this_id, this_size);
          Array.set block_map i ((-1), this_size);
          loop (j + 1) (pred i)
        | None ->
          (* loop (pred i) *)
          block_map
  in
  loop 0 (Array.length block_map - 1)

(** Calculate the checksum of a block map. *)
let checksum block_map =
  Array.mapi (fun i (id, _) -> if id = (-1) then 0 else i * id) block_map |> (Array.fold_left ( + ) 0)

let run () =
  let block_map = read_input "./input/09.txt" in
  Printf.printf "Day 9: Disk Fragmenter\n";
  Printf.printf " checksum = %d\n" (defragment block_map |> checksum);
  (* Printf.printf "number of resonant antinodes = %d\n" (count_antinodes antenna_map true) *)
