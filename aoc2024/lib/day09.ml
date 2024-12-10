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
  let rec loop id offset inodes free_blks l =
    match l with
    | [] -> (inodes, free_blks)
    | [blocks] -> (InodeSet.add (Inode.make id offset blocks) inodes, free_blks)
    | file :: free :: others ->
        let new_fs = InodeSet.add (Inode.make id offset file) inodes in
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
  
let update_fs inodes free_blks (inode : Inode.t) (free : FreeBlock.t) =
  match compare inode.len free.len with
  | -1 -> 
    let new_inodes =
      InodeSet.(remove inode inodes |> add (Inode.make inode.id free.start inode.len)) in
    let new_free_blks = 
      FreeBlockSet.(remove free free_blks |> add (FreeBlock.make inode.start inode.len) |>
                    add (FreeBlock.make (free.start + inode.len) (free.len - inode.len))) in
    (new_inodes, new_free_blks)
  | 0 ->
    let new_inodes = InodeSet.(remove inode inodes |> add (Inode.make inode.id free.start inode.len)) in
    let new_free_blks = FreeBlockSet.(remove free free_blks |> add (FreeBlock.make inode.start inode.len)) in
    (new_inodes, new_free_blks)
  | _ ->
    let new_inodes = 
      InodeSet.(remove inode inodes |> add (Inode.make inode.id free.start free.len) |>
                add (Inode.make inode.id inode.start (inode.len - free.len))) in
    let new_free_blks = 
      FreeBlockSet.(remove free free_blks |> add (FreeBlock.make (inode.start + inode.len - free.len) free.len)) in
    (new_inodes, new_free_blks)

(** Take a file system [(InodeSet.t * FreeBlockSet.t)]) and defragment it. *)
let defragment fs =
  let rec loop (inodes, free_blks) =
    let inode = InodeSet.find_last (fun _ -> true) inodes in
    let free = FreeBlockSet.find_first (fun _ -> true) free_blks in
    if inode.start < free.start then
      (inodes, free_blks)
    else
      loop (update_fs inodes free_blks inode free)
  in
  loop fs

(** Return a [Seq] that contains the integers starting from [s] and including 
    [len] numbers. *)
let range s len =
  Seq.init len (fun i -> s + i)

(** Calculate the checksum of a the filesystem [fs]. *)
let checksum (inodes, _) =
  let node_checksum id start len = range start len |> (Seq.fold_left ( + ) 0) |> (( * ) id) in
  InodeSet.fold (fun {id = id; start = s; len = l} checksum -> checksum + node_checksum id s l) inodes 0

let run () =
  let fs = read_input "./input/09.txt" in
  Printf.printf "Day 9: Disk Fragmenter\n";
  Printf.printf " checksum = %d\n" (defragment fs |> checksum);
  (* Printf.printf "number of resonant antinodes = %d\n" (count_antinodes antenna_map true) *)

