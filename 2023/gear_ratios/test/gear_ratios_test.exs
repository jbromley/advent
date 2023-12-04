defmodule GearRatiosTest do
  use ExUnit.Case
  doctest GearRatios

  test "sum engine parts" do
    data = [
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]

    assert GearRatios.sum_part_numbers(data) == 4361
  end
end
