(* Day 21: Keypad Conundrum *)
open Utils

let of_string s =
  String.split_on_char '\n' s

let string_of_char = String.make 1
    
let make_key_map s =
  let seq = String.split_on_char '\n' s |> List.to_seq |> Seq.map String.to_seq in
  let map = Hashtbl.create (String.length s) in
  Seq.iteri
    (fun y row ->
       Seq.iteri
         (fun x ch ->
            Hashtbl.add map ch (x, y))
         row)
    seq;
  map

let num_pad = Board.of_string "789\n456\n123\n 0A"
let num_pad_dict = make_key_map "789\n456\n123\n 0A"

let dir_pad = Board.of_string " ^A\n<v>"
let dir_pad_dict = make_key_map " ^A\n<v>"

let path_to_dirs path = 
  let rec aux p1 path dirs =
    match path with
    | [] -> String.concat "" (List.rev dirs)
    | p2 :: ps ->
      let dir =
        match Coord.sub p2 p1 with
        | (1, 0) -> ">"
        | (-1, 0) -> "<"
        | (0, 1) -> "v"
        | (0, -1) -> "^"
        | _ -> failwith "path_to_dirs: invalid move"
      in
      aux p2 ps (dir :: dirs)
  in
  aux (List.hd path) (List.tl path) []

let code_to_int (code : string) : int =
  String.to_seq code
  |> Seq.filter (fun ch -> '0' <= ch && ch <= '9')
  |> String.of_seq
  |> int_of_string

let bfs_all_paths (grid : char Board.t) ((xs, ys) : Coord.t) ((xf, yf) : Coord.t) : (int * int) list list =
  let width, height = Board.size grid in
  let distances = Array.make_matrix height width max_int in
  let predecessors = Array.make_matrix height width [] in
  let rec bfs q =
    if Queue.is_empty q then
      if distances.(yf).(xf) = max_int then None
      else Some predecessors
    else
      let x, y = Queue.pop q in
      let current_distance = distances.(y).(x) in
      (* Printf.printf "(%d, %d), distance %d\n" x y current_distance; *)
      List.iter
        (fun (dx, dy) ->
           let xn, yn = Coord.add (x, y) (dx, dy) in
           if Board.contains grid (xn, yn) && Board.at grid (xn, yn) <> ' ' then
             let new_distance = current_distance + 1 in
             (* Printf.printf "  (%d, %d), distance %d\n" xn yn new_distance; *)
             if new_distance < distances.(yn).(xn) then (
               distances.(yn).(xn) <- new_distance;
               predecessors.(yn).(xn) <- [(x, y)];
               Queue.add (xn, yn) q)
             else if new_distance = distances.(y).(x) then
               let prev_preds = predecessors.(y).(x) in
               predecessors.(yn).(xn) <- (x, y) :: prev_preds)
        [(0, 1); (1, 0); (-1, 0); (0, -1)];
      (* Printf.printf "  queue length = %d\n" (Queue.length q); *)
      bfs q
  in
  let rec build_paths ((x, y) as pos) predecessors =
    if pos = (xs, ys) then
      [[(xs, ys)]]
    else
      List.fold_left
        (fun acc predecessor ->
           List.fold_left
             (fun acc path ->
                (path @ [(x, y)]) :: acc)
             acc
             (build_paths predecessor predecessors))
        []
        predecessors.(y).(x)
  in
  let q = Queue.create () in 
  Queue.add (xs, ys) q;
  distances.(ys).(xs) <- 0;
  match bfs q with
  | None -> []
  | Some predecessors -> build_paths (xf, yf) predecessors
    
let find_button_sequence_length (code : string)  (robots : int) : int =
  (code_to_int code) * robots
  
  (* (\* Build up the sequence of number and direction pads to be used. *\) *)
  (* let pad_last_pos = Array.make (robots + 1) (fun i -> if i = 0 then Hashtbl.find num_pad_dict 'A' *)
  (*                                              else Hashtbl.find dir_pad_dict 'A') in *)
  (* let pads = Array.make (robots + 1) (fun i -> if i = 0 then num_pad else dir_pad) in *)
  (* let pad_dicts = Array.make (robots + 1) (fun i -> if i = 0 then num_pad_dict else dir_pad_dict) in *)
  (* let seq_cache = Hashtbl.create 128 in *)
  (* let rec find_sequence_length code phase = *)
  (*   if Hashtbl.mem seq_cache (code, phase) then Hashtbl.find seq_cache (code, phase) *)
  (*   else *)
  (*     let best_path_len =  *)
  (*       String.fold_left *)
  (*         (fun path_len ch -> *)
  (*            path_len + *)
  (*            List.fold_left *)
  (*              (fun acc path -> *)
  (*                 pad_last_pos.(phase) <- new_pos; *)
  (*                 let cur_path_len = List.length path in *)
  (*                 if phase < robots then *)
  (*                   cur_path_len = find_sequence_length path (phase + 1) *)
  (*                 else *)
  (*                   min cur_path_len acc) *)
  (*              max_int *)
  (*              (bfs_all_paths_as_dirs *)
  (*                 (if phase = 0 then 0 else 1) *)
  (*                 pads.(phase) *)
  (*                 pad_last_pos.(phase) *)
  (*                 Hashtbl.find pad_dicts.(phase) ch)) *)
  (*         0 *)
  (*         code *)
  (*     in *)
  (*     Hashtbl.add seq_cache (code, phase) best_path_len; *)
  (* in *)
  (* find_sequence_length code 0 *)
  
let compute_complexity (codes : string list) : int =
  List.fold_left
    (fun acc code ->
       let seq_len = find_button_sequence_length code 2 in
       let code_int = code_to_int code in
       acc + seq_len * code_int)
    0
    codes

let run () =                   
  let _codes = Io.read_file "./input/21.txt" |> of_string in
  Printf.printf "Day 21: Keypad Conundrum";
  (* Printf.printf "cheats (radius = 2) = %d\n" (count_cheats track 2 100); *)
  (* Printf.printf "cheats (radius = 20) = %d\n" (count_cheats track 20 100); *)
