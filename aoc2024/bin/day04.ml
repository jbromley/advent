(* Day 4: Ceres Search *)
    
(** Read a file and return an array of strings. *)
let read_string_array name : string array =
  let ic = open_in name in 
  let try_read () =
    try
      Some(input_line ic)
    with End_of_file -> None in
  let rec loop input =
    match try_read () with
    | Some s ->
      loop (s :: input)
    | None ->
      close_in ic; List.rev input |> Array.of_list in
  loop []

(** Check if the four-letter horizontal word starting at the given row and
    column is "XMAS". Check in both the forward and backwards directions.
    Return 1 if it is "XMAS" and 0 otherwise. *)
let is_horiz_xmas cw row col =
  try
    let word = String.sub (cw.(row)) col 4 in
    if word = "XMAS" || word = "SAMX" then 1 else 0
  with Invalid_argument(_) -> 0

(** Check if the four-letter vertical word starting at the given row and
    column is "XMAS". Check in both the forward and backwards directions.
    Return 1 if it is "XMAS" and 0 otherwise. *)
let is_vert_xmas cw row col =
  let rec get_char s r n =
    match n with
    | 0 -> s
    | _ ->
      try
        let ch = String.sub cw.(r) col 1 in
        get_char (s ^ ch) (r + 1) (pred n)
      with Invalid_argument(_) -> ""
  in
  let word = get_char "" row 4 in
  if word = "XMAS" || word = "SAMX" then 1 else 0

(** Reverse a string. *)
let reverse_string s =
  let n = String.length s in
  let result = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set result i s.[n - i - 1]
  done;
  Bytes.to_string result

(** Check if the four-letter diagonal (down and left or right, depending on
    col_delta) word starting at the given row and column is the given word.
    Check in both the forward and backwards directions. Return 1 if it matches
    word and 0 otherwise. *)
let is_diag_word cw row col col_delta word =
  let rec get_char s r c n =
    match n with
    | 0 -> s
    |_ ->
      try
        let ch = String.sub cw.(r) c 1 in
        get_char (s ^ ch) (succ r) (c + col_delta) (pred n)
      with Invalid_argument(_) -> ""
  in
  let w = get_char "" row col (String.length word) in
  if w = word || w = (reverse_string word) then 1 else 0

(** Count forward and backward occurrences of "XMAS" that start at the given
    row and column. *)
let count_xmas cw row col =
  let ch = cw.(row).[col] in
  if ch = 'X' || ch = 'S' then
    (* Any XMAS (forward or backwards) must start with 'X' or 'S'. *)
    is_vert_xmas cw row col +
    is_horiz_xmas cw row col +
    is_diag_word cw row col (-1) "XMAS" +
    is_diag_word cw row col 1 "XMAS"
  else
    0

(** Part 1: Count all occurrences of "XMAS" in the crossword. *)
let count_all_xmas cw =
  let max_row = Array.length cw - 1 in
  let max_col = String.length cw.(0) - 1 in
  let rec count_one r c n =
    match r, c with
    | (-1, _) -> n
    | (r, -1) -> count_one (pred r) max_col n
    | (r, c) -> count_one r (pred c) (n + count_xmas cw r c)
  in
  count_one max_row max_col 0

(** Count "MAS" crosses with the center at the given row and column. *)
let count_mas_crosses cw row col =
  let ch = cw.(row).[col] in
  if ch = 'A' then
    (* There must be an 'A' in the middle of the cross. *)
    if is_diag_word cw (row - 1) (col - 1) 1 "MAS" = 1 &&
       is_diag_word cw (row - 1) (col + 1) (-1) "MAS" = 1 then
      1
    else
      0
  else
    0

(** Part 2: Count all "MAS" crosses in the crossword. *)
let count_all_mas_crosses cw =
  let max_row = Array.length cw - 1 in
  let max_col = String.length cw.(0) - 1 in
  let rec count_one r c n =
    match r, c with
    | (-1, _) -> n
    | (r, -1) -> count_one (pred r) max_col n
    | (r, c) -> count_one r (pred c) (n + count_mas_crosses cw r c)
  in
  count_one max_row max_col 0
  
let run () =
  let input = read_string_array "./input/04.txt" in 
  Printf.printf "Day 3: Mull It Over\n";
  Printf.printf "count \"XMAS\" = %d\n" (count_all_xmas input);
  Printf.printf "count \"MAS\" crosses = %d\n" (count_all_mas_crosses input)
