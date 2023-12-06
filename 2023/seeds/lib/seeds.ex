defmodule Seeds do
  @moduledoc """
  Solution for Advent of Code 2023 day 5 - If You Give A Seed A Fertilizer.
  """
  @map_re ~r/(\d+)/

  # Part 1

  def find_seed_soil(input_file) do
    [seed_list | maps_in] = read_input(input_file)

    seeds =
      Regex.scan(~r/(\d+)/, seed_list, capture: :all_but_first)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)

    maps = parse_maps(maps_in)

    seeds |> Enum.map(&trace_seed(&1, maps)) |> Enum.min()
  end

  def read_input(input_file) do
    input_file |> File.read!() |> String.split("\n", trim: true)
  end

  def parse_maps(input, acc \\ {[], []}) do
    {maps, cur_map} = acc

    case input do
      [] ->
        [cur_map | maps] |> Enum.reject(&Enum.empty?/1) |> Enum.reverse()

      [line | rest] ->
        case Regex.scan(@map_re, line, capture: :all_but_first) do
          [] ->
            parse_maps(rest, {[Enum.reverse(cur_map) | maps], []})

          map_nums ->
            [dst, src, len] = map_nums |> List.flatten() |> Enum.map(&String.to_integer/1)
            parse_maps(rest, {maps, [{dst, src, src + len - 1} | cur_map]})
        end
    end
  end

  def trace_seed(item, maps) do
    case maps do
      [] -> item
      [map | maps] -> trace_seed(lookup(map, item), maps)
    end
  end

  def lookup(map, item) do
    map_item = map |> Enum.find(nil, fn {_d, s, e} -> item >= s && item <= e end)

    case map_item do
      nil -> item
      {dst, src, _} -> dst + item - src
    end
  end
end
