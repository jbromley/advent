defmodule BoatRace do
  @moduledoc """
  Solution for Advent of Code 2023 day 6 - Wait for It
  """

  # Part 1

  def count_all_wins(input_file) do
    read_input(input_file)
    |> Enum.map(fn {t, r} -> count_wins_fast(t, r) end)
    |> Enum.product()
  end

  def read_input(input_file) do
    File.read!(input_file)
    |> String.split("\n", trim: true)
    |> Enum.map(fn s ->
      Regex.scan(~r/(\d+)/, s, capture: :all_but_first)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.zip()
  end

  # Part 2

  def count_long_race_wins(input_file) do
    [time, record] = read_input_bad_kerning(input_file)
    count_wins_fast(time, record)
  end

  def read_input_bad_kerning(input_file) do
    File.read!(input_file)
    |> String.split("\n", trim: true)
    |> Enum.map(fn s ->
      String.split(s, ":") |> Enum.at(1) |> String.replace(" ", "") |> String.to_integer()
    end)
  end

  # Common code 

  def count_wins(time, record) do
    0..time
    |> Enum.map(fn t -> t * (time - t) end)
    |> Enum.count(fn distance -> distance > record end)
  end

  def count_wins_fast(time, record) do
    Stream.iterate(time - 1, fn n -> n - 2 end)
    |> Enum.reduce_while({1, 0}, fn inc, {n, total} ->
      new_total = total + inc
      if new_total > record, do: {:halt, time + 1 - 2 * n}, else: {:cont, {n + 1, new_total}}
    end)
  end
end
