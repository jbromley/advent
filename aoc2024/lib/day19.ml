(* Day 18: Linen Layout *)
open Utils

let of_string s =
  match String.trim s |> String.split_on_char '\n' with
  | ps :: "" :: designs ->
    ( String.split_on_char ',' ps |> List.map String.trim, designs)
  | _ -> failwith "bad input string"

let chop_prefix prefix s =
  if String.starts_with ~prefix:prefix s then
    let prefix_length = String.length prefix in
    let s_length = String.length s in
    Some (String.sub s prefix_length (s_length - prefix_length))
  else
    None
    
let is_design_possible patterns design =
  let rec aux design memo =
    let len = String.length design in
    if len = 0 then true
    else
      match memo.(len) with
      | -1 -> false
      | 1 -> true
      | _ ->
        List.exists
          (fun pattern ->
             match chop_prefix pattern design with
             | Some design' ->
               if aux design' memo then
                 (memo.(len) <- 1; true)
               else
                 (memo.(len) <- -1; false)
             | None ->
               memo.(len) <- -1;
               false)
          patterns
  in
  aux design (Array.make (String.length design + 1) 0)

let count_possible_designs patterns designs =
  List.filter
    (fun design -> is_design_possible patterns design)
    designs
  |> List.length

let possible_arrangements patterns design =
  let rec aux design memo =
    let len = String.length design in
    (* Printf.printf "len = %d, design = \"%s\"\n" len design; *)
    (* Printf.printf "memo = %s\n" (Debug.int_array_to_string memo); *)
    match memo.(len) with
    | -1 -> 0
    | n when n > 0 -> n
    | _ ->
      List.fold_left
        (fun acc pattern ->
           match chop_prefix pattern design with
           | Some design' ->
             let acc' = acc + aux design' memo in
             (* Printf.printf "acc' = %d\n" acc'; *)
             memo.(len) <- acc';
             acc'
           | None ->
             memo.(len) <- if acc = 0 then -1 else acc;
             acc
        )
        0
        patterns
  in
  let memo = Array.make (String.length design + 1) 0 in
  memo.(0) <- 1;
  aux design memo
    
let count_arrangements patterns designs =
  List.fold_left
    (fun acc design -> acc + possible_arrangements patterns design)
    0
    designs
        
let run () =                   
  let patterns, designs = Io.read_file "./input/19.txt" |> of_string in
  Printf.printf "Day 19: Linen Layout\n";
  Printf.printf "possible designs = %d\n" (count_possible_designs patterns designs);
  Printf.printf "different arrangements = %d\n" (count_arrangements patterns designs)
