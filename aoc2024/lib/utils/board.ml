type 'a t = 'a array array

let of_string s =
  List.fold_left
    (fun acc line -> (String.to_seq line |> Array.of_seq) :: acc)
    []
    (String.split_on_char '\n' s) |> List.rev |> Array.of_list

let size b =
  (Array.length b.(0), Array.length b)

let at b (x, y) =  b.(y).(x)
let at_opt b (x, y) =
  let w, h = size b in
  if 0 <= x && x < w && 0 <= y && y < h then Some b.(y).(x)
  else None

let set b (x, y) data = b.(y).(x) <- data

let to_string b =
  Array.fold_left
    (fun acc row -> (Array.to_seq row |> String.of_seq) :: acc)
    []
    b |> List.rev |> String.concat "\n"


