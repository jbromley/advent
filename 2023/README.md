# Advent of Code 2023

## Index

| Day | Title               | Language |
|-----|---------------------|----------|
|   1 | trebuchet           | Elixir   |
|   2 | cube_conundrum      | Elixir   |
|   3 | gear_ratios         | Elixir   |
|   4 | scratchcards        | Elixir   |
|   5 | seeds               | Elixir   | 
|   6 | boat_race           | Elixir   |
|   7 | camel_cards         | Elixir   |
|   8 | wasteland           | Elixir   |
|   9 | oasis               | Elixir   |
|  10 | pipe_maze           | Elixir   |
|  11 | cosmic-expansion    | Racket   |
|  12 | hot-springs         | Racket   |
|  13 | mirrors             | Racket   |
|  15 | lens-library        | Racket   |
|  16 | lava                | Racket   |
|  17 | clumsy-crucible     | Racket   |
|  18 | parabolic-reflector | Racket   |
|  19 | aplenty             | Racket   |
|  20 | pulse-propagation   | Racket   |
|  21 | step-counter        | Racket   |
|  22 | sand-slabs          | Racket   |
|  23 | long-walk           | Racket   |
|  24 | odds                | Racket   |
|  25 | snowverload         | Racket   |
|-----|---------------------|----------|

## Running the Elixir code
To run this code, you will need [Elixir](https://elixir-lang.org/) 1.15
installed. I recommend using [rtx](https://github.com/jdx/rtx) to install
and use Elixir. It is a handy tool for installing different versions of a
large number of languages and tools.

Once you have Elixir installed, navigate to the directory for the code/day you
want to run and run the following commands. This example shows how to run 
Trebuchet, but the method is the same for all the days. 
```
mix escript.build
./trebuchet input
```

## Running the Racket

[Racket](https://racket-lang.org) is a "batteries-included" Scheme variant.
To run the Racket code, you will need to have Racket installed. On Ubuntu, 
Dr Racket (the integrated Racket runtime and IDE) can be installed by running 

```
sudo apt install racket
```

Once you have racket installed, you can run a Racket program by using 
`racket <source-file>`. For example, to run the Hot Springs code, navigate
into the `hot-springs` directory and run the following command.

```
racket hot-springs.rkt
```

