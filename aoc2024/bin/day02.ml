(* Day 2: Red-Nosed Reports *)

(** Read a file and return a list of lists of integers. *)
let read_lists name : int list list =
  let ic = open_in name in 
  let try_read () =
    try
      Some (input_line ic)
    with End_of_file -> None in
  let rec loop ll =
    match try_read () with
    | Some s ->
      let l = String.split_on_char ' ' s |> (List.map (fun x -> int_of_string x)) in
      loop (l :: ll)
    | None ->
      close_in ic; ll in
  loop []

(** Determine if a report is safe. For a report to be safe, it must be all
    increasing or all decreasing and the difference between any two levels
    must be 1, 2, or 3. *)
let is_report_safe l =
  let rec loop l last dir =
    match l with
    | [] -> true
    | x :: xs when dir = 0 ->
      let diff = List.hd xs - x in
      if abs(diff) >= 1 && abs(diff) <= 3 && diff <> 0 then
        loop xs x (diff / abs(diff))
      else
        false
    | x :: xs ->
      let diff = x - last in
      let direction = if diff = 0 then 0 else diff / abs(diff) in 
      if abs(diff) >= 1 && abs(diff) <= 3 && direction = dir then
        loop xs x dir
      else
        false
  in
  loop l 0 0

(** Part 1: How many safe reports are there. *)
let count_safe_reports ll =
  List.map is_report_safe ll |> (List.filter_map (fun x -> if x then Some(x) else None)) |> List.length

(** Given a list, generate all lists that result from dropping a single
    element of the original list. *)
let generate_damped_reports lst =
  let rec loop acc prefix = function
    | [] -> acc
    | x :: xs -> loop((prefix @ xs) :: acc) (prefix @ [x]) xs
  in
  loop [] [] lst

(** Given a failed report, generate all damped reports and test if any
    of those reports are safe. If any damped report is safe return true,
    otherwise return false. *)
let check_damped_reports r =
  let rec check_reports = function
    | [] -> false
    | r :: rs ->
      if is_report_safe r then true else check_reports rs
  in
  generate_damped_reports r |> check_reports
  
(** Part 2: How many safe reports with damping. *)
let count_safe_damped_reports report_list = 
  let safe_undamped = List.map is_report_safe report_list in
  let safe_damped = List.map2 (fun is_safe input -> if is_safe then true else check_damped_reports input) safe_undamped report_list in
  safe_damped |>  (List.filter_map (fun x -> if x then Some(x) else None)) |> List.length
  
let run () =
  let input = read_lists "./input/2.txt" in 
  Printf.printf "Day 2: Red-Nosed Reports\n";
  Printf.printf "safe reports = %d\n" (count_safe_reports input);
  Printf.printf "safe damped reports = %d\n" (count_safe_damped_reports input);
