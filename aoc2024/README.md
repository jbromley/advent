# Advent of Code 2024

This repo contains solutions to Advent of Code 2024 puzzles in [OCaml](https://ocaml.org/).

## Problems

| Day | Problem            | Runtime (ms) |
| --- | ------------------ | ------------ |
|  1  | Historian Hysteria |          3.2 |
|  2  | Red-Nosed Reports  |          2.0 |
|  3  | Mull It Over       |          2.1 |
|  4  | Ceres Search       |          4.1 |
|  5  | Print Queue        |         15.2 |
|  6  | Guard Gallivant    |       6475.2 |
|  7  | Bridge Repair      |       6679.9 |
| --- | ------------------ | ------------ |

## Problem input 

Note that input files are **not** included in this repo. If you would like to
run this code with your input, the input should be placed in the `input`
directory, named according to the day. For example, `day01.ml` expects to find 
`input/01.txt` for its puzzle input.

## Problem commentary

### Day 1: Historian Hysteria

This was a nice problem to ease into the Advent of Code. The `frequencies`
function for tabulating the number of occurrences of each item in a list
will likely be useful in future problems.

### Day 2: Red-Nosed Reports

Part 1 was relatively easy as long as one made sure to process the first two
numbers in the list properly. For part 2, when a report was unsafe, I simply
generated all possible "damped" reports and tested each one in turn until I
found there was a safe damped report or I reached the end of the damped
reports list. No effort was used to avoid generating all possible damped
reports from where the unsafe sequence occurred in the original report.

### Day 3: Mull It Over

This problem was regular expressions all the way down. Part 2 involved a
recursive functions state machine. If there is one thing that is terrible
about regular expressions in OCaml (and in most languages) it is figuring
out how many backslashes you need to deal properly with regular expressions
inside a string. Elixir did it right by having a sigil (`~r`)that allowed you to
create a regular expressions without using a string, so no backslash
doubling problems.

### Day 4: Ceres Search

Pretty much a brute force solution. For part 1, iterate over every position
and look for "XMAS" or "SAMX" to the right, down, diagonal down and left,
and diagonal down and right. Checking for both "XMAS" and "SAMX" means I
never had to explicity look left, up, or diagonally up. Any instance of
"XMAS" must start with either an 'X' or an 'S' so only check those
locations. Part 2 was similar. An 'A' had to be in the middle of the X, so
only check locations that have an 'A'.

### Day 5: Print Queue

For part 1, my first instinct was to build a page precedence graph and then use
this to check page orders, but this ignored the part about not using rules that
didn't have both numbers from an update. It turns out that just making a set of
the rules and then for each number pair of numbers checking to make sure that
there was an appropriate rule for those two numbers.

Part 2 changed the update to a set of numbers. While the set isn't empty, find a
number that goes first according to the rules. Add this number to the result,
then remove it from the set and iterate.

### Day 6: Guard Gallivant

For part 1, I just stepped through the guard's path saving visited positions in
a set. There was some nice use of modules and types to encapsulate code for
positions, positions and directions, and the map itself.

For part 2, find each position visited, try placing an obstruction there, and
then test for a cycle in the guard's path. Remember to check both position and
direction to detect cycles.

Up to this point, no puzzle took more than about 15 ms, this one took 6475 ms. I
basically brute-forced it, but maybe there is a better way.

### Day 7: Bridge Repair

First-order functions made it easy to implement an evaluator. For both parts,
generate and test every possible combination of operators and then test them
with the numbers from the equations to see which equations could be made true.

For part 2, I did the slimy thing and simply redefined OCaml's `||` operator to
be a numeric concatenation function. This made it easy to extend the code to use
three (or an arbitrary number of `(int -> int -> int)`) operators.
