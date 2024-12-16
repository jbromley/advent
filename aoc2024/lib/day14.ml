(* Day 14: Restroom Redoubt *)

module IntPair = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
end

module IntPairSet = Set.Make(IntPair)

let read_input name =
  let ic = open_in name in 
  let try_read () =
    try Some (input_line ic)
    with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some s ->loop (s :: lines)
    | None -> close_in ic; List.rev lines in
  loop []

type robot = { pos: int * int; vel: int * int }
             
let parse_input lines =
  List.map
    (fun s ->
       Scanf.sscanf s "p=%d,%d v=%d,%d" (fun x y vx vy -> { pos = (x, y); vel = (vx, vy) })) lines

let step_robot { pos = (x, y); vel = (vx, vy) } (w, h) =
  let wrap x y =
    let new_x = if x < 0 then x + w else if x >= w then x - w else x in
    let new_y = if y < 0 then y + h else if y >= h then y - h else y in
    (new_x, new_y)
  in 
  let pos' = wrap (x + vx) (y + vy) in
  { pos = pos'; vel = (vx, vy) }

let step_robots robots steps map_size =
  List.fold_left
    (fun acc _i -> List.map (fun r -> step_robot r map_size) acc)
    robots
    (List.init steps (fun i -> i))

let in_region ((min_x, min_y), (max_x, max_y)) { pos=(x, y); vel=_ } =
  x >= min_x && x <= max_x && y >= min_y && y <= max_y

let count_quadrant quad robots =
  List.filter (fun r -> in_region quad r) robots |> List.length
                                                      
let calc_safety_factor robots (w, h) =
  let hw = w / 2 in
  let hh = h / 2 in
  let quadrants =
    [((0, 0), (hw - 1, hh - 1));
     ((hw + 1, 0), (w - 1, hh - 1));
     ((hw + 1, hh + 1), (w - 1, h - 1));
     ((0, hh + 1), (hw - 1, h - 1))]
  in
  List.fold_left (fun acc q -> acc * count_quadrant q robots) 1 quadrants

let safety_factor robots steps map_size =
  calc_safety_factor
    (step_robots robots steps map_size)
    map_size

let in_tree robots =
  (* Get positions of all robots to simplify processing. *)
  let positions = List.map (fun { pos = pos; vel = _ } -> pos) robots |> IntPairSet.of_list in
  IntPairSet.exists
    (fun (x, y) ->
       List.for_all
         (fun dx -> IntPairSet.mem (x + dx, y) positions)
         (List.init 10 (fun i -> i)))
    positions

let find_tree robots map_size =
  let rec loop robots steps =
    if in_tree robots then steps
    else loop (step_robots robots 1 map_size) (steps + 1)
  in
  loop robots 0
        
let run () =
  let robots = read_input "./input/14.txt" |> parse_input in
  let map_size = (101, 103) in
  Printf.printf "Day 14: Restroom Redoubt\n";
  Printf.printf "safety factor = %d\n" (safety_factor robots 100 map_size);
  Printf.printf "find tree = %d\n" (find_tree robots map_size)
