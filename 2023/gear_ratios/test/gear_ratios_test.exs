defmodule GearRatiosTest do
  use ExUnit.Case
  doctest GearRatios

  @data [
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

  test "sum engine parts" do
    assert GearRatios.sum_part_numbers(@data) == 4361
  end

  test "sum gear ratios" do
    assert GearRatios.sum_gear_ratios(@data) == 467_835
  end
end
