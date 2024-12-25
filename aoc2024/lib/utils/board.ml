type 'a t = 'a array array

let of_string s =
  List.fold_left
    (fun acc line -> (String.to_seq line |> Array.of_seq) :: acc)
    []
    (String.trim s |> String.split_on_char '\n') |> List.rev |> Array.of_list

let size b =
  (Array.length b.(0), Array.length b)

let contains b (x, y) =
  let width, height = size b in
  0 <= x && x < width && 0 <= y && y < height
                                   
let at b (x, y) =  b.(y).(x)

let at_opt b (x, y) =
  let w, h = size b in
  if 0 <= x && x < w && 0 <= y && y < h then Some b.(y).(x)
  else None

let set b (x, y) data = b.(y).(x) <- data

let find_first b data =
  let y_size = Array.length b in
  let rec aux y =
    if y = y_size then failwith "Board.find_first: element not found"
    else
      match Array.find_index (fun elt -> elt = data) b.(y) with
      | Some x -> (x, y)
      | None -> aux (y + 1)
  in
  aux 0

let find_first_opt b data =
  let y_size = Array.length b in
  let rec aux y =
    if y = y_size then None
    else
      match Array.find_index (fun elt -> elt = data) b.(y) with
      | Some x -> Some (x, y)
      | None -> aux (y + 1)
  in
  aux 0

let find_all b data =
  let y_size = Array.length b in
  let rec aux y acc =
    if y = y_size then acc
    else
      match Array.find_index (fun elt -> elt = data) b.(y) with
      | Some x -> aux (y + 1) ((x, y) :: acc)
      | None -> aux (y + 1) acc
  in
  aux 0 []

let counti (b : 'a t) (f : Coord.t -> 'a -> bool) : int =
  let x_size, y_size = size b in
  let rec aux x y count =
    if y = y_size then count
    else if x = x_size then aux 0 (y + 1) count
    else aux (x + 1) y (if f (x, y) (at b (x, y)) then (count + 1) else count)
  in
  aux 0 0 0

let to_string b =
  Array.fold_left
    (fun acc row -> (Array.to_seq row |> String.of_seq) :: acc)
    []
    b |> List.rev |> String.concat "\n"
