# Advent of Code 2024

This repo contains solutions to Advent of Code 2024 puzzles in [OCaml](https://ocaml.org/).

## Problems

| Day | Problem            | Runtime (ms) |
| --- | ------------------ | ------------ |
|  1  | Historian Hysteria |          3.2 |
|  2  | Red-Nosed Reports  |          2.0 |
|  3  | Mull It Over       |          2.1 |
|  4  | Ceres Search       |          4.1 |
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
