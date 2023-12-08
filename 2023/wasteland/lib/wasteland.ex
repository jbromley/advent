defmodule Wasteland do
  @moduledoc """
  Solution for Advent of Code 2023, day 8 - Haunted Wasteland.
  """

  # Part 1

  def navigate_map(input_file) do
    {turns, nodes} = read_input(input_file)

    Stream.cycle(turns)
    |> Enum.reduce_while({"AAA", 0}, fn turn, {node, count} ->
      if node == "ZZZ", do: {:halt, count}, else: {:cont, {lookup(nodes, node, turn), count + 1}}
    end)
  end

  # Common code

  def read_input(input_file) do
    [turns | nodes] =
      File.read!(input_file)
      |> String.split("\n", trim: true)

    turns = String.split(turns, "", trim: true)

    nodes =
      nodes
      |> Enum.map(&Regex.scan(~r/([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)/, &1, capture: :all_but_first))
      |> List.flatten()
      |> Enum.chunk_every(3)
      |> List.foldl(%{}, fn [n, l, r], nodes -> Map.put(nodes, n, {l, r}) end)

    {turns, nodes}
  end

  def lookup(nodes, node, turn) do
    turns = Map.get(nodes, node)
    if turn == "L", do: elem(turns, 0), else: elem(turns, 1)
  end
end
