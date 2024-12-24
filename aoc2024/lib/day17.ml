(* Day 17: Chronospatial Computer *)
open Utils

type computer = {
  program: int array;
  ip : int;
  a : int;
  b : int;
  c : int;
  output : int list
}

let of_string s =
  let next_int s si =
    let i = 2 + String.index_from s si ':' in
    let nl = String.index_from s i '\n' in
    let len = nl - i in
    (int_of_string (String.sub s i len), nl + 1)
  in
  let a, n = next_int s 0 in
  let b, n = next_int s n in
  let c, n = next_int s n in
  let pindex = 2 + String.index_from s n ':' in
  let ptext = String.sub s pindex (String.length s - pindex) |> String.trim in
  let program =
    String.split_on_char ',' ptext
    |> List.map (fun s -> int_of_string s)
    |> Array.of_list
  in
  { program; ip = 0; a; b; c; output = [] }

let literal_operand { program; ip; _ } =
  program.(ip + 1)

let combo_operand { program; ip; a; b; c; _ } =
  match program.(ip + 1) with
  | (0 | 1 | 2 | 3) as n -> n
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | op -> failwith ("combo_operand: bad operand " ^ (string_of_int op))

let pdiv x y = x / int_of_float (2.0 ** float_of_int y)

let adv ({ ip; a; _ } as comp) =
  let a' = pdiv a (combo_operand comp) in
  { comp with ip = ip + 2; a = a' }

let bxl ({ip; b; _} as comp) =
  let b' = b lxor (literal_operand comp) in
  { comp with ip = ip + 2; b = b' }

let bst ({ip; _} as comp) =
  let b' = (combo_operand comp) mod 8 in
  { comp with ip = ip + 2; b = b' }

let jnz ({ip; a; _} as comp) =
  if a = 0 then
    {comp with ip = ip + 2}
  else
    { comp with ip = literal_operand comp }

let bxc ({ip; b; c; _} as comp) =
  let b' = b lxor c in
  { comp with ip = ip + 2; b = b' }

let out ({ip; output; _} as comp) =
  let out = (combo_operand comp) mod 8 in
  { comp with ip = ip + 2; output = out :: output }

let bdv ({ ip; a; _ } as comp) =
  let b' = pdiv a (combo_operand comp) in
  { comp with ip = ip + 2; b = b' }

let cdv ({ ip; a; _ } as comp) =
  let c' = pdiv a (combo_operand comp) in
  { comp with ip = ip + 2; c = c' }

let decode = function
  | 0 -> adv
  | 1 -> bxl
  | 2 -> bst
  | 3 -> jnz
  | 4 -> bxc
  | 5 -> out
  | 6 -> bdv
  | 7 -> cdv
  | n -> failwith ("step: bad opcode " ^ string_of_int n)

let step ({program; ip; _} as c) =
  let opcode = program.(ip) in
  decode opcode c
                 
let run comp =
  let rec aux c =
    if c.ip = Array.length c.program then
      List.rev c.output |> List.map string_of_int |> String.concat ","
    else
      aux (step c)
  in
  aux comp

let range first last =
  List.init (last - first + 1) (fun i -> i)
    
let find_quine ({ program; _ } as comp) =
    let loop c =
      List.fold_left
        (fun acc _ -> match acc with None -> None | Some c -> Some (step c))
        (Some c)
        (range 0 8)
    in
    let get_as start end_b =
      Printf.printf "get_as %d %d\n" start end_b;
      List.filter
        (fun a ->
           let comp = loop { comp with a } in
           match comp with
           | Some comp -> comp.b mod 8 = end_b
           | None -> false)
        (range start (start + 8))
    in
    let rec aux prev_a step_num =
      Printf.printf "step number %d\n" step_num;
      if step_num = -1 then Some prev_a
      else
        let goal = program.(step_num) in
        let temp = get_as (prev_a * 8) goal in
        Printf.printf "goal = %d\n" goal;
        List.find_map
          (fun a -> match aux a (step_num - 1) with None -> None | Some a -> Some a)
          temp
    in
    aux 0 5 |> Option.value ~default:0
  
let run () =
  let input = Io.read_file "./input/17.txt" |> of_string in
  Printf.printf "Day 17: Chronospatial Computer\n";
  Printf.printf "output (part 1) = %s\n" (run input);
  (* Printf.printf "tiles on best path = %d\n" (count_path_tiles maze) *)
