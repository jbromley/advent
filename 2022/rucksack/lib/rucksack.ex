defmodule Rucksack do
  @moduledoc """
  Day 3 of 2022 Advent of Code
  """
  @lower_case ~c"abcdefghijklmnopqrstuvwxyz"
  @upper_case ~c"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def mispacked_priority(input_file) do
    rucksacks = read_rucksacks(input_file)

    rucksacks
    |> Enum.map(&find_common_items(&1))
    |> Enum.map(fn items -> List.foldl(items, 0, fn item, acc -> acc + priority(item) end) end)
    |> Enum.reduce(fn priority, acc -> acc + priority end)
  end

  def badge_priority(input_file) do
    read_rucksacks(input_file)
    |> Enum.map(&items_to_set(&1))
    |> Enum.chunk_every(3, 3, :discard)
    |> Enum.map(&find_group_badge(&1))
    |> List.foldl(0, fn item, total_priority -> total_priority + priority(item) end)
  end

  def find_common_items(rucksack) do
    {compartment1, compartment2} = String.split_at(rucksack, String.length(rucksack) |> div(2))
    items1 = items_to_set(compartment1)
    items2 = items_to_set(compartment2)
    MapSet.intersection(items1, items2) |> MapSet.to_list()
  end

  def find_group_badge(group) do
    group
    |> Enum.reduce(fn items, common_items -> MapSet.intersection(items, common_items) end)
    |> MapSet.to_list()
    |> Enum.at(0)
  end

  def priority(item) do
    cond do
      item in @lower_case -> 1 + item - ?a
      item in @upper_case -> 27 + item - ?A
      true -> 0
    end
  end

  defp read_rucksacks(input_file) do
    {:ok, rucksacks} = File.read(input_file)
    rucksacks |> String.split("\n") |> Enum.drop(-1)
  end

  defp items_to_set(items) do
    items |> String.to_charlist() |> MapSet.new()
  end
end
