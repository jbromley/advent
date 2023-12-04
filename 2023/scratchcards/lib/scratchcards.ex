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
    |> Map.to_list()
    |> List.foldl(0, fn card, points ->
      matches = elem(wins(card), 0)
      points + if matches > 0, do: 2 ** (matches - 1), else: 0
    end)
  end

  # Part 2 

  def solve_part_2(input_file) do
    read_input(input_file) |> parse_cards() |> count_cards()
  end

  def count_cards(cards) do
    num_cards = cards |> Map.to_list() |> Enum.count()
    total_cards = Enum.zip(1..num_cards, List.duplicate(1, num_cards)) |> Map.new()

    wins =
      cards
      |> Map.to_list()
      |> Enum.map(&wins/1)
      |> Enum.sort(&(elem(&1, 1) <= elem(&2, 1)))

    count_cards(num_cards, total_cards, wins)
  end

  def count_cards(num_cards, total_cards, wins) do
    case wins do
      [] ->
        total_cards |> Map.values() |> Enum.sum()

      [{num_wins, card} | rest] ->
        if num_wins > 0 && card < num_cards do
          added_cards = Map.get(total_cards, card)

          last_card = min(num_cards, card + num_wins)

          new_total_cards =
            (card + 1)..last_card
            |> Range.to_list()
            |> List.foldl(total_cards, fn c, t -> Map.update!(t, c, &(&1 + added_cards)) end)

          count_cards(num_cards, new_total_cards, rest)
        else
          count_cards(num_cards, total_cards, rest)
        end
    end
  end

  # Common code

  def wins({card, {winning, mine}}) do
    num_wins = MapSet.intersection(winning, mine) |> Enum.count()
    {num_wins, card}
  end

  def read_input(input_file) do
    File.read!(input_file) |> String.split("\n", trim: true)
  end

  def parse_cards(input) do
    input
    |> Enum.map(&parse_card(&1))
    |> Map.new()
  end

  defp parse_card(input) do
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
