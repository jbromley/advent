(* Day 13: Claw Contraption *)

let read_input name =
  let ic = open_in name in 
  let try_read () =
    try
      Some (input_line ic)
    with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some s ->
      if s <> "" then loop (s :: lines)
      else loop lines
    | None ->
      close_in ic; List.rev lines in
  loop []

let rec split l n =
  if n <= 0 then
    ([], l) (* Base case: if n is 0 or less, first list is empty *)
  else
    match l with
    | [] -> ([], []) (* If the list is empty, both splits are empty *)
    | hd :: tl ->
      let (left, right) = split tl (n - 1) in
      (hd :: left, right) (* Add the current head to the "left" split *)

let parse_input input =
  let parse_line l =
    let re = Str.regexp "X[\\+=]\\([0-9]+\\), Y[\\+=]\\([0-9]+\\)" in
    let _ = Str.search_forward re l 0 in
    let x = Str.matched_group 1 l |> int_of_string in
    let y = Str.matched_group 2 l |> int_of_string in
    (x, y)
  in 
  let rec loop acc lines =
    if List.length lines < 3 then acc
    else
      let eq, others = split lines 3 in
      loop
        ((List.nth eq 0 |> parse_line,
          List.nth eq 1 |> parse_line,
          List.nth eq 2 |> parse_line)
         :: acc)
        others
  in
  loop [] input |> List.rev

let make_input2 input =
  List.map
    (fun (a, b, (x, y)) -> (a, b, (x + 10000000000000, y + 10000000000000)))
    input

(*
   | qa + rb = x
   | sa + tb = y

   Eliminating b from the above equations yields

   a = (tx - ry) / (qt - rs), b = (y - sa) / t
*)    
let solve ((q, s), (r, t), (x, y)) max_moves =
  let a = (t * x - r * y) / (q * t - r * s) in
  let b = (y - s * a) / t in
  let rx, ry = (q * a + r * b, s * a + t * b) in
  if (rx, ry) = (x, y) && a < max_moves && b < max_moves then Some (a * 3 + b)
  else None

let sum_win_costs input max_moves =
  List.filter_map (fun game -> solve game max_moves) input |> List.fold_left (+) 0 

let run () =                   
  let input = read_input "./input/13.txt" |> parse_input in
  let input2 = input |> make_input2 in
  Printf.printf "Day 13: Claw Contraption\n";
  Printf.printf "tokens spend (part 1) = %d\n" (sum_win_costs input 100);
  Printf.printf "count stones (part 2) = %d\n" (sum_win_costs input2 max_int)
