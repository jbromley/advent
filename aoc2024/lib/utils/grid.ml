type 'a t = 'a array array

let of_string s =
  String.trim s
  |> String.split_on_char '\n'
  |> List.fold_left (fun acc line -> (String.to_seq line |> Array.of_seq) :: acc) []
  |> List.rev
  |> Array.of_list

let size grd =
  (Array.length grd.(0), Array.length grd)

let contains grd (x, y) =
  let width, height = size grd in
  0 <= x && x < width && 0 <= y && y < height
                                   
let at grd (x, y) =  grd.(y).(x)

let at_opt grd (x, y) =
  let w, h = size grd in
  if 0 <= x && x < w && 0 <= y && y < h then Some grd.(y).(x)
  else None

let set grd (x, y) data = grd.(y).(x) <- data

let neighbors grd pos =
  List.filter
    (fun cell -> contains grd cell)
    (Coord.neighbors pos)

let find_first grd data =
  let y_size = Array.length grd in
  let rec aux y =
    if y = y_size then failwith "Grid.find_first: element not found"
    else
      match Array.find_index (fun elt -> elt = data) grd.(y) with
      | Some x -> (x, y)
      | None -> aux (y + 1)
  in
  aux 0

let find_first_opt grd data =
  let y_size = Array.length grd in
  let rec aux y =
    if y = y_size then None
    else
      match Array.find_index (fun elt -> elt = data) grd.(y) with
      | Some x -> Some (x, y)
      | None -> aux (y + 1)
  in
  aux 0

let find_all grd data =
  let y_size = Array.length grd in
  let rec aux y acc =
    if y = y_size then acc
    else
      match Array.find_index (fun elt -> elt = data) grd.(y) with
      | Some x -> aux (y + 1) ((x, y) :: acc)
      | None -> aux (y + 1) acc
  in
  aux 0 []

let counti (grd : 'a t) (f : Coord.t -> 'a -> bool) : int =
  let x_size, y_size = size grd in
  let rec aux x y count =
    if y = y_size then count
    else if x = x_size then aux 0 (y + 1) count
    else aux (x + 1) y (if f (x, y) (at grd (x, y)) then (count + 1) else count)
  in
  aux 0 0 0

let to_string grd =
  Array.fold_left
    (fun acc row -> (Array.to_seq row |> String.of_seq) :: acc)
    []
    grd |> List.rev |> String.concat "\n"
