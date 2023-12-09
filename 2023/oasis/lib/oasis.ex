defmodule Oasis do
  @moduledoc """
  Solution for Advent of Code 2023, day 8 - Mirage Maintenance.
  """

  # Part 1
  def forecast_all(input_file) do
    stream_file(input_file) |> Stream.map(&forecast/1) |> Enum.sum()
  end

  def forecast(seq) do
    seq |> reduce_sequence() |> extrapolate_sequence()
  end

  # Part 2
  def backcast_all(input_file) do
    stream_file(input_file)
    |> Stream.map(&Enum.reverse/1)
    |> Stream.map(&forecast/1)
    |> Enum.sum()
  end

  # Common Code

  def reduce_sequence(seq) do
    seq |> Enum.reverse() |> reduce_sequence([])
  end

  def reduce_sequence(seq, reductions) do
    case Enum.all?(seq, &(&1 == 0)) do
      true ->
        reductions

      false ->
        diffs = Enum.chunk_every(seq, 2, 1, :discard) |> Enum.map(fn [x1, x2] -> x1 - x2 end)
        reduce_sequence(diffs, [seq | reductions])
    end
  end

  def extrapolate_sequence([seq | seqs]) do
    [inc | _] = seq
    extrapolate_sequence(seqs, inc)
  end

  def extrapolate_sequence([seq | seqs], inc) do
    case seqs do
      [] ->
        inc + Enum.at(seq, 0)

      _ ->
        [elem0 | _] = seq
        extrapolate_sequence(seqs, elem0 + inc)
    end
  end

  def stream_file(input_file) do
    File.stream!(input_file)
    |> Stream.map(fn line ->
      String.split(line, ~r/ |\n/, trim: true) |> Enum.map(&String.to_integer/1)
    end)
  end
end
