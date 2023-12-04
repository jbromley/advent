defmodule Scratchcards do
  @moduledoc """
  Solutions for Advent of Code 2023, day 4 - Scratchcards.
  """

  # Part 1

  def solve_part_1(input_file) do
    read_input(input_file) |> parse_cards() |> count_points()
  end

  def count_points(cards) do
    cards
    |> Map.values()
    |> List.foldl(0, fn {win, mine}, points ->
      matches = MapSet.intersection(win, mine) |> Enum.count()
      points + if matches > 0, do: 2 ** (matches - 1), else: 0
    end)
  end

  # Common code

  def read_input(input_file) do
    File.read!(input_file) |> String.split("\n", trim: true)
  end

  def parse_cards(input) do
    input
    |> Enum.map(&parse_card(&1))
    |> Map.new()
  end

  def parse_card(input) do
    [card, numbers] = input |> String.split(":", trim: true)
    card_id = card |> String.slice(5, 3) |> String.trim() |> String.to_integer()
    [winning_numbers, my_numbers] = parse_numbers(numbers)
    {card_id, {winning_numbers, my_numbers}}
  end

  defp parse_numbers(numbers_str) do
    numbers_str
    |> String.split("|", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(fn l -> Enum.map(l, fn n -> String.to_integer(n) end) |> MapSet.new() end)
  end
end
