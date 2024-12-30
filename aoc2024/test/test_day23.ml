open OUnit2
open Aoc
open Day23

let g = {|kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn|} |> of_string

let test_count_3_cliques _ = assert_equal 7 (count_3_cliques g)
let test_find_password _ = assert_equal "co,de,ka,ta" (find_password g)
  
let suite_name = "Day 23: LAN Party"
let suite =
  suite_name >::: [
    "test count 3-cliques" >:: test_count_3_cliques;
    "test find password" >:: test_find_password
  ]

let () =
  Printf.printf "Testing %s...\n" suite_name;
  run_test_tt_main suite
