module Coord = struct 
  type t = int * int
  type coordinate = t

  let equal (x1, y1) (x2, y2) =
    x1 = x2 && y1 = y2

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c

  let hash = Hashtbl.hash

  let neighbors (x, y) = [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
  let offsets = neighbors (0, 0)

  let rotate_cw (x, y) = (-y, x)
  let rotate_180 (x, y) = (-x, -y)
  let rotate_ccw (x, y) = (y, -x)

  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  let mul c (x, y) = (c * x, c * y)

  let to_string (x, y) =
    Printf.sprintf "(%d, %d)" x y
end

include Coord

module Set = Set.Make(Coord)
module Map = Map.Make(Coord)
