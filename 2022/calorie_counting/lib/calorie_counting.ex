defmodule CalorieCounting do
  @moduledoc """
  Advent of Code day 1 - calorie counting.   \"""

  @doc \"""
  Given a list of calorie values carried by each elf, find the maximum 
  number of calories.
  """
  def maximum_calories(input_file) do
    {:ok, calorie_data} = File.read(input_file)

    count_elf_calories(calorie_data)
    |> Enum.max()
  end

  def top_three_total(input_file) do
    {:ok, calorie_data} = File.read(input_file)

    count_elf_calories(calorie_data)
    |> Enum.sort(&(&1 >= &2))
    |> Enum.take(3)
    |> Enum.sum()
  end

  def count_elf_calories(calorie_data) do
    calorie_data
    |> String.split("\n")
    |> List.foldl(
      {0, []},
      fn calories, {total_calories, calorie_list} ->
        case calories do
          "" -> {0, [total_calories | calorie_list]}
          _ -> {total_calories + String.to_integer(calories), calorie_list}
        end
      end
    )
    |> elem(1)
  end
end
