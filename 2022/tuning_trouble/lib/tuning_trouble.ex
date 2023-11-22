defmodule TuningTrouble do
  @moduledoc """
  Solution for day 6 of the 2022 Advent of Code.
  """

  def read_input(input_file) do
    File.read!(input_file) |> String.to_charlist() |> Enum.drop(-1)
  end

  def find_packet_start(signal) do
    lock(signal, 4)
  end

  def find_message_start(signal) do
    pkt_start = find_packet_start(signal)
    lock(Enum.drop(signal, pkt_start), 14, pkt_start + 14)
  end

  defp lock(signal, lock_chars) do
    lock(signal, lock_chars, lock_chars)
  end

  defp lock(signal, lock_chars, pos) do
    case signal |> Enum.take(lock_chars) |> count_symbols() do
      ^lock_chars -> pos
      _ -> lock(Enum.drop(signal, 1), lock_chars, pos + 1)
    end
  end

  defp count_symbols(signal) do
    signal |> Enum.frequencies() |> Map.keys() |> Enum.count()
  end
end
