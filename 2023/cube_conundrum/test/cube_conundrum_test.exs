defmodule CubeConundrumTest do
  use ExUnit.Case
  doctest CubeConundrum

  @input [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

  test "test impossible games sum" do
    games = CubeConundrum.parse_input(@input)
    assert CubeConundrum.find_possible_games(games) == 8
  end

  test "test sum of minimal powers" do
    games = CubeConundrum.parse_input(@input)
    assert CubeConundrum.total_minimal_power(games) == 2286
  end
end
