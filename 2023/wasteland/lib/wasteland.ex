defmodule Wasteland do
  @moduledoc """
  Solution for Advent of Code 2023, day 8 - Haunted Wasteland.
  """

  # Part 1

  def navigate_map(input_file) do
    {turns, map} = read_input(input_file)

    Stream.cycle(turns)
    |> Enum.reduce_while({"AAA", 0}, fn turn, {node, count} ->
      if node == "ZZZ", do: {:halt, count}, else: {:cont, {lookup(map, node, turn), count + 1}}
    end)
  end

  # Part 2

  def navigate_map_ghostlike(input_file) do
    {turns, map} = read_input(input_file)
    start_nodes = Enum.filter(Map.keys(map), fn node -> String.ends_with?(node, "A") end)

    # start_nodes
    # |> Enum.map(&find_path_length(map, turns, &1))
    # |> Enum.reduce(&lcm/2)
    pmap(start_nodes, &find_path_length(map, turns, &1))
    |> Enum.reduce(&lcm/2)
  end

  defp find_path_length(map, turns, start_node) do
    Stream.cycle(turns)
    |> Enum.reduce_while({start_node, 0}, fn turn, {node, count} ->
      if String.ends_with?(node, "Z"),
        do: {:halt, count},
        else: {:cont, {lookup(map, node, turn), count + 1}}
    end)
  end

  def pmap(collection, fun) do
    collection
    |> Enum.map(&Task.async(fn -> fun.(&1) end))
    |> Enum.map(&Task.await(&1))
  end

  def lcm(a, b) do
    div(a * b, Integer.gcd(a, b))
  end

  # Common code

  def read_input(input_file) do
    [turns | nodes] =
      File.read!(input_file)
      |> String.split("\n", trim: true)

    turns = String.split(turns, "", trim: true)

    nodes =
      nodes
      |> Enum.map(
        &Regex.scan(~r/([A-Z|0-9]+) = \(([A-Z|0-9]+), ([A-Z|0-9]+)\)/, &1,
          capture: :all_but_first
        )
      )
      |> List.flatten()
      |> Enum.chunk_every(3)
      |> List.foldl(%{}, fn [n, l, r], nodes -> Map.put(nodes, n, {l, r}) end)

    {turns, nodes}
  end

  defp lookup(nodes, node, turn) do
    turns = Map.get(nodes, node)
    if turn == "L", do: elem(turns, 0), else: elem(turns, 1)
  end
end
