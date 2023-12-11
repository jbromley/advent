defmodule PipeMaze do
  @moduledoc """
  Solution for Advent of Code 2023, day 10 - Pipe Maze.
  """

  # Part 1

  def find_farthest(input_file) do
  end

  # Common code

  def read_input(input_file) do
    map =
      File.stream!(input_file)
      |> Stream.map(&String.trim/1)
      |> Stream.map(&string_to_array/1)
  end

  def string_to_array(l) do
    len = String.length(l)
    a = :array.from_list(String.to_charlist(l))
    :array.resize(len, a)
    :array.fix(a)
    a
  end
end
