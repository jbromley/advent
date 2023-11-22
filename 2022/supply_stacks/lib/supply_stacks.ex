defmodule SupplyStacks do
  @moduledoc """
  Solution for day 5 of the 2022 Advent of Code
  """
  def read_input(input_file) do
    lines = File.read!(input_file) |> String.split("\n")

    # Find blank line that separates stack description and moves
    index = Enum.find_index(lines, fn s -> s == "" end)

    # Parse the stack description into a map of lists
    stacks = Enum.take(lines, index - 1) |> Enum.reverse() |> parse_stacks()

    # Parse the move strings into moves
    moves = Enum.slice(lines, (index + 1)..-2//1) |> Enum.map(&parse_move(&1))

    {stacks, moves}
  end

  def move_stacks(input_file) do
    {stacks, moves} = read_input(input_file)

    moves
    |> List.foldl(stacks, fn %{num: n, start_pos: s, end_pos: e}, stks ->
      move_boxes(stks, n, s, e)
    end)
    |> Map.to_list()
    |> List.keysort(0)
    |> Enum.map_join("", fn {_stack, boxes} -> List.first(boxes) end)
  end

  def multi_move_stacks(input_file) do
    {stacks, moves} = read_input(input_file)

    moves
    |> List.foldl(stacks, fn %{num: n, start_pos: s, end_pos: e}, stks ->
      move_multi_boxes(stks, n, s, e)
    end)
    |> Map.to_list()
    |> List.keysort(0)
    |> Enum.map_join("", fn {_stack, boxes} -> List.first(boxes) end)
  end

  def move_boxes(stacks, num, start_pos, end_pos) do
    case num do
      0 -> stacks
      _ -> move_boxes(move_box(stacks, start_pos, end_pos), num - 1, start_pos, end_pos)
    end
  end

  def move_multi_boxes(stacks, num, start_pos, end_pos) do
    {moved_boxes, remaining_boxes} = Map.get(stacks, start_pos) |> Enum.split(num)

    stacks
    |> Map.replace!(start_pos, remaining_boxes)
    |> Map.update!(end_pos, fn stack -> moved_boxes ++ stack end)
  end

  defp move_box(stacks, start_pos, end_pos) do
    [box | rest_stack] = Map.get(stacks, start_pos)

    stacks
    |> Map.replace!(start_pos, rest_stack)
    |> Map.update!(end_pos, fn stack -> [box | stack] end)
  end

  defp parse_stacks(stacks_desc, stack \\ %{}) do
    case stacks_desc do
      [] ->
        stack

      [this_level | other_levels] ->
        new_stack =
          parse_stack_level(this_level)
          |> List.foldl(stack, fn {stack_num, box}, stack ->
            Map.update(stack, stack_num, [box], fn boxes -> [box | boxes] end)
          end)

        parse_stacks(other_levels, new_stack)
    end
  end

  defp parse_stack_level(stack_level) do
    re = ~r/(?<=\[)[[:alpha:]](?=\])/

    Regex.scan(re, stack_level, return: :index)
    |> List.flatten([])
    |> Enum.map(fn {index, _} -> {div(index, 4) + 1, String.at(stack_level, index)} end)
  end

  defp parse_move(move) do
    re = ~r/^move (?<num>\d+) from (?<start_pos>\d) to (?<end_pos>\d)$/

    Regex.named_captures(re, move)
    |> Map.new(fn {k, v} -> {String.to_atom(k), String.to_integer(v)} end)
  end
end
