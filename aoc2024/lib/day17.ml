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
  | n -> failwith ("decode: bad opcode " ^ string_of_int n)

let step ({program; ip; _} as c) =
  let opcode = program.(ip) in
  decode opcode c
                 
let exec comp =
  let rec aux c =
    if c.ip = Array.length c.program then
      List.rev c.output
    else
      aux (step c)
  in
  aux comp

let exec_to_string comp =
  let output = exec comp in
  List.map string_of_int output |> String.concat ","

let range first last =
  List.init (last - first + 1) (fun i -> i)

let rec take n l =
  match l, n with
  | _, 0 -> []
  | [], _ -> []
  | x :: xs, _ -> x :: take (n - 1) xs

let find_quine ({ program; _ } as comp) =
  let goal = Array.to_list program |> List.rev in
  let goal_length = List.length goal in 
  let rec aux q poss =
    if Queue.is_empty q then
      List.fold_left (fun x acc -> if x < acc then x else acc) max_int poss
    else
      let outputs, r = Queue.pop q in
      if outputs <= goal_length then
        let next_as =
          Seq.map (fun a -> (outputs + 1, (r lsl 3) lor a)) (Seq.init 8 (fun i -> i))
          |> Seq.filter (fun (_, a') -> List.rev (exec { comp with a = a' }) = (take outputs goal))
        in
        Queue.add_seq q next_as;
        aux q poss
      else if List.rev (exec { comp with a = r }) = goal then
        aux q (r :: poss)
      else
        aux q poss
  in
  let q = Queue.create () in
  Queue.push (1, 0) q;
  aux q []
  
let run () =
  let input = Io.read_file "./input/17.txt" |> of_string in
  Printf.printf "Day 17: Chronospatial Computer\n";
  Printf.printf "output (part 1) = %s\n" (exec_to_string input);
  Printf.printf "quine when a = %d\n" (find_quine input)
