defmodule Roshambo do
  @moduledoc """
  Day 2 of 2022 Advent of Code.
  """

  def calculate_naive_guide_score(input_file) do
    read_games(input_file)
    |> Enum.map(fn game -> wrong_game_points(game) end)
    |> Enum.sum()
  end

  def calculate_guide_score(input_file) do
    read_games(input_file)
    |> Enum.map(fn game -> game_points(game) end)
    |> Enum.sum()
  end

  defp read_games(input_file) do
    {:ok, games} = File.read(input_file)
    games |> String.split("\n")
  end

  defp wrong_game_points("A X"), do: 4
  defp wrong_game_points("A Y"), do: 8
  defp wrong_game_points("A Z"), do: 3
  defp wrong_game_points("B X"), do: 1
  defp wrong_game_points("B Y"), do: 5
  defp wrong_game_points("B Z"), do: 9
  defp wrong_game_points("C X"), do: 7
  defp wrong_game_points("C Y"), do: 2
  defp wrong_game_points("C Z"), do: 6
  defp wrong_game_points(_), do: 0

  defp game_points("A X"), do: 3
  defp game_points("A Y"), do: 4
  defp game_points("A Z"), do: 8
  defp game_points("B X"), do: 1
  defp game_points("B Y"), do: 5
  defp game_points("B Z"), do: 9
  defp game_points("C X"), do: 2
  defp game_points("C Y"), do: 6
  defp game_points("C Z"), do: 7
  defp game_points(_), do: 0
end
