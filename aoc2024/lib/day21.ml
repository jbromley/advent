(* Day 21: Keypad Conundrum *)
open Utils

let of_string s =
  String.trim s |> String.split_on_char '\n'

let code_to_int (code : string) : int =
  String.to_seq code
  |> Seq.filter (fun ch -> '0' <= ch && ch <= '9')
  |> String.of_seq
  |> int_of_string

let make_key_map s =
  let seq = String.split_on_char '\n' s |> List.to_seq |> Seq.map String.to_seq in
  let map = Hashtbl.create (String.length s) in
  Seq.iteri
    (fun y row ->
       Seq.iteri
         (fun x ch ->
            if ch <> ' ' then Hashtbl.add map ch (x, y))
         row)
    seq;
  map

let generate_moves start finish vert_first =
  let dx, dy = Coord.sub finish start in
  let aux delta ch_inc ch_dec moves =
    moves ^ String.init (abs delta) (fun _ -> if delta > 0 then ch_dec else ch_inc)
  in
  if vert_first then aux dy '^' 'v' "" |> aux dx '<' '>' |> fun s -> s ^ "A"
  else aux dx '<' '>' "" |> aux dy '^' 'v' |> fun s -> s ^ "A"

let generate_num_pad_moves ((x0, y0) as start) ((x1, y1) as finish) =
  let vert_first = (y0 = 3 && x1 = 0) || ((not (x0 = 0 && y1 = 3)) && x0 <= x1) in
  generate_moves start finish vert_first

let generate_dir_pad_moves ((x0, y0) as start) ((x1, y1) as finish) =
  let vert_first = (y0 = 0 && x1 = 0) || ((not (x0 = 0 && y1 = 0)) && x0 <= x1) in
  generate_moves start finish vert_first

let generate_move_cache cells f_gen =
  let keys = Hashtbl.to_seq_keys cells |> List.of_seq in
  let num_cells = List.length keys in
  let cache = Hashtbl.create (num_cells * num_cells) in
  List.iter
    (fun start_key ->
       List.iter
         (fun end_key ->
            let moves = f_gen (Hashtbl.find cells start_key) (Hashtbl.find cells end_key) in
            Hashtbl.add cache (start_key, end_key) moves)
         keys)
    keys;
  cache

let get_num_pad_moves =
  let num_pad_cells = make_key_map "789\n456\n123\n 0A" in 
  let cache = generate_move_cache num_pad_cells generate_num_pad_moves in
  let rec build_moves last_key keys moves =
    match keys with
    | [] -> List.rev moves |> String.concat ""
    | key :: other_keys ->
      let new_moves = Hashtbl.find cache (last_key, key) in
      build_moves key other_keys (new_moves :: moves)
  in
  fun code ->
    let keys = List.init (String.length code) (String.get code) in 
    build_moves 'A' keys []

let split_moves s =
  let rec aux acc start =
    if start >= String.length s then List.rev acc
    else
      match String.index_from_opt s start 'A' with
      | Some i -> aux (String.sub s start (i - start + 1) :: acc) (i + 1)
      | None -> failwith "split_moves: move without terminating 'A'"
  in
  aux [] 0

let get_dir_pad_moves =
  let dir_pad_cells = make_key_map " ^A\n<v>" in
  let cache = generate_move_cache dir_pad_cells generate_dir_pad_moves in
  let rec build_moves last_key keys moves =
    match keys with
    | [] -> List.rev moves |> String.concat ""
    | key :: other_keys ->
      let new_moves = Hashtbl.find cache (last_key, key) in
      build_moves key other_keys (new_moves :: moves)
  in
  fun dir_moves ->
    let keys = List.init (String.length dir_moves) (String.get dir_moves) in
    build_moves 'A' keys []
    
let count_steps =
  let move_cache = Hashtbl.create 128 in
  let rec count c dir_pads =
    if dir_pads = 0 then String.length c
    else if c = "A" then 1
    else
      let key = (dir_pads, c) in
      if Hashtbl.mem move_cache key then Hashtbl.find move_cache key
      else
        let total_steps =
          List.fold_left
            (fun acc m -> acc + count (get_dir_pad_moves m) (dir_pads - 1))
            0
            (split_moves c)
        in
        Hashtbl.add move_cache key total_steps;
        total_steps
  in
  fun moves dir_pads -> count moves dir_pads

let compute_complexity (codes : string list) (num_dir_pads : int) : int =
  List.fold_left
    (fun acc code ->
       let seq_len = count_steps (get_num_pad_moves code) num_dir_pads in
       let code_int = code_to_int code in
       acc + seq_len * code_int)
    0
    codes

let run () =                   
  let codes = Io.read_file "./input/21.txt" |> of_string in
  Printf.printf "Day 21: Keypad Conundrum\n";
  Printf.printf "complexity (2 direction pads) = %d\n" (compute_complexity codes 2);
  Printf.printf "complexity (25 direction pads) = %d\n" (compute_complexity codes 25);

