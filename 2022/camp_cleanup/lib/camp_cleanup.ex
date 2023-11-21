defmodule CampCleanup do
  @moduledoc """
  Day 4 of the 2022 Advent of Code
  """

  def count_duplicate_assignments(input_file) do
    read_assignments(input_file)
    |> Enum.map(&fully_contained_assignments?(&1))
    |> Enum.count(& &1)
  end

  def count_overlapping_assignments(input_file) do
    input_file
    |> read_assignments()
    |> Enum.map(&overlapping_assignments?(&1))
    |> Enum.count(& &1)
  end

  defp fully_contained_assignments?([{s1, e1}, {s2, e2}]) do
    (s1 >= s2 and e1 <= e2) or (s2 >= s1 and e2 <= e1)
  end

  defp overlapping_assignments?([{s1, e1}, {s2, e2}]) do
    s1 <= e2 and e1 >= s2
  end

  defp read_assignments(input_file) do
    {:ok, contents} = File.read(input_file)

    contents
    |> String.split("\n")
    |> Enum.drop(-1)
    |> Enum.map(&parse_assignments(&1))
  end

  defp parse_assignments(assignments) do
    assignments
    |> String.split(",")
    |> Enum.map(fn a ->
      String.split(a, "-") |> Enum.map(&String.to_integer(&1)) |> List.to_tuple()
    end)
  end
end
