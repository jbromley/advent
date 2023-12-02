defmodule CubeConundrum do
  @moduledoc """
  Solution for 2023 Advent of Code - Cube Conundrum.
  """
  @max_cubes %{
    "red" => 12,
    "green" => 13,
    "blue" => 14
  }

  def solve_part_1() do
    "input" |> read_input() |> parse_input() |> find_possible_games()
  end

  def find_possible_games(games) do
    games
    |> Map.to_list()
    |> List.foldl(0, fn {id, game}, acc -> if is_game_possible?(game), do: acc + id, else: acc end)
  end

  @spec is_game_possible?([%{String.t() => integer()}]) :: boolean()
  def is_game_possible?(game) do
    game |> Enum.all?(&is_subset_possible?/1)
  end

  @spec is_subset_possible?(%{String.t() => integer()}) :: boolean()
  def is_subset_possible?(subset) do
    subset |> Map.to_list() |> Enum.all?(fn {color, qty} -> qty <= Map.get(@max_cubes, color) end)
  end

  @spec read_input(String.t()) :: list(String.t())
  def read_input(input_file) do
    File.read!(input_file) |> String.split("\n", trim: true)
  end

  @spec parse_input(list(String.t())) :: %{integer() => [%{String.t() => integer()}]}
  def parse_input(input) do
    input |> Enum.map(&parse_game/1) |> Map.new()
  end

  @spec parse_game(String.t()) :: {integer(), [%{String.t() => integer()}]}
  def parse_game(line) do
    [id, subsets] = line |> String.split(":", trim: true)
    {parse_id(id), parse_subsets(subsets)}
  end

  @spec parse_id(String.t()) :: integer()
  def parse_id(game_id) do
    game_id |> String.slice(5..-1//1) |> String.to_integer()
  end

  @spec parse_subsets(String.t()) :: [%{String.t() => integer()}]
  def parse_subsets(subsets) do
    subsets |> String.split(";") |> Enum.map(&parse_subset/1)
  end

  @spec parse_subset(String.t()) :: %{String.t() => integer()}
  def parse_subset(subset) do
    subset
    |> String.split(",")
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(fn [qty, color] -> {color, String.to_integer(qty)} end)
    |> Map.new()
  end
end
