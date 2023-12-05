import pathlib
from functools import reduce

puzzle_input = pathlib.Path("./input").read_text().split("\n\n")
puzzle_input = [line.split("\n") for line in puzzle_input]


def seed_locations(puzzle_input):
    seeds = {int(seed): int(seed) for seed in puzzle_input[0][0].split(":")[1].split()}
    maps = puzzle_input[1:]
    for m in maps:
        for seed in seeds.keys():
            f = False
            seed_value = seeds[seed]
            for line in m[1:]:
                print(line)
                d, s, r = [int(x) for x in line.split()]
                if seed_value >= s and seed_value < (s + r) and f == False:
                    seeds[seed] = d + (seed_value - s)
                    f = True

    return min(s for s in seeds.values())


output = seed_locations(puzzle_input)
print(output)
