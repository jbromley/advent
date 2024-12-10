(* Day 9: Disk Fragmenter *)

module Inode = struct
  type t = { id: int; start: int; len: int }
  let make id s l = {id = id; start = s; len = l}
  let compare {id = id1; start = s1; len = _} {id = id2; start = s2; len = _} =
    match compare s1 s2 with
    | 0 -> compare id1 id2
    | other -> other
end

module InodeSet = Set.Make(Inode)

module FreeBlock = struct
  type t = { start: int; len: int }
  let make s l = {start = s; len = l}
  let compare {start = s1; len = _} {start = s2; len = _} =
    compare s1 s2
end

module FreeBlockSet = Set.Make(FreeBlock)

(** Take the [int list] and build the filesystem descriptio. Returns
    [(InodeSet.t, FreeBlockSet.t)]. *)
let build_block_maps lst =
  let rec loop id offset fs free_blks l =
    match l with
    | [] -> (fs, free_blks)
    | [blocks] -> (InodeSet.add (Inode.make id offset blocks) fs, free_blks)
    | file :: free :: others ->
        let new_fs = InodeSet.add (Inode.make id offset file) fs in
        let new_free_blks = FreeBlockSet.add (FreeBlock.make (offset + file) free) free_blks in 
        loop (id + 1) (offset + file + free) new_fs new_free_blks others
  in
  loop 0 0 InodeSet.empty FreeBlockSet.empty lst

(** Read the input file with name [filename] and return a filesystem
    ([InodeSet.t]) and a free block list [FreeBlockSet.t]. *)
let read_input filename =
  let ic = open_in filename in
  let line = input_line ic in
  close_in ic;
  String.to_seq line |> List.of_seq |> List.map (fun ch -> Char.code ch - Char.code '0') |> build_block_maps
  
let update_fs fs free_blks (file : Inode.t) (free : FreeBlock.t) =
  match compare file.len free.len with
  | -1 -> 
    let new_fs =
      InodeSet.remove file fs |>
      InodeSet.add (Inode.make file.id free.start file.len) in
    let new_free_blks = 
      FreeBlockSet.remove free free_blks |> 
      FreeBlockSet.add (FreeBlock.make file.start file.len) |>
      FreeBlockSet.add (FreeBlock.make (free.start + file.len) (free.len - file.len)) in
    (new_fs, new_free_blks)
  | 0 ->
    let new_fs = InodeSet.remove file fs |> InodeSet.add (Inode.make file.id free.start file.len) in
    let new_free_blks = FreeBlockSet.remove free free_blks |> FreeBlockSet.add (FreeBlock.make file.start file.len) in
    (new_fs, new_free_blks)
  | _ ->
    let new_fs = 
      InodeSet.remove file fs |> 
      InodeSet.add (Inode.make file.id free.start free.len) |>
      InodeSet.add (Inode.make file.id file.start (file.len - free.len)) in
    let new_free_blks = 
      FreeBlockSet.remove free free_blks |>
      FreeBlockSet.add (FreeBlock.make (file.start + file.len - free.len) free.len) in
    (new_fs, new_free_blks)

(** Take a block array [(int * int) array]) with the id and size of blocks and
    defragment it. *)
let defragment (fs : InodeSet.t) (free_blks : FreeBlockSet.t) =
  let rec loop fs free_blks =
    let file = InodeSet.find_last (fun _ -> true) fs in
    let free = FreeBlockSet.find_first (fun _ -> true) free_blks in
    if file.start < free.start then
      fs
    else
      let (new_fs, new_free_blks) = update_fs fs free_blks file free in
      loop new_fs new_free_blks
  in
  loop fs free_blks

(** Return a [Seq] that contains the integers starting from [s] and including 
    [len] numbers. *)
let range s len =
  Seq.init len (fun i -> s + i)

(** Calculate the checksum of a the filesystem [fs]. *)
let checksum fs =
  let node_checksum id start len = range start len |> (Seq.fold_left ( + ) 0) |> (( * ) id) in
  InodeSet.fold (fun {id = id; start = s; len = l} checksum -> checksum + node_checksum id s l) fs 0

(*
let run () =
  let (fs, free_blks) = read_input "./input/09.txt" in
  Printf.printf "Day 9: Disk Fragmenter\n";
  Printf.printf " checksum = %d\n" (defragment fs free_blks |> checksum);
  (* Printf.printf "number of resonant antinodes = %d\n" (count_antinodes antenna_map true) *)
*)
