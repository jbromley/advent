defmodule Seeds do
  @moduledoc """
  Solution for Advent of Code 2023 day 5 - If You Give A Seed A Fertilizer.
  """
  @map_re ~r/(\d+)/

  def main(args) do
    if Enum.count(args) != 1 do
      IO.puts("Usage: seeds input_file")
      System.halt(1)
    end

    input = Enum.at(args, 0)

    IO.puts("Advent of Code 2023, day 5 - If You Give a Seed a Fertilizer")
    IO.puts("Part 1: #{find_seed_location(input)}")
    IO.puts("Part 2: #{find_seed_location_intervals(input)}")
  end

  # Part 1

  def find_seed_location(input_file) do
    [seed_list | maps_in] = read_input(input_file)

    seeds =
      Regex.scan(~r/(\d+)/, seed_list, capture: :all_but_first)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)

    maps = parse_maps(maps_in)

    seeds |> Enum.map(&trace_seed(&1, maps)) |> Enum.min()
  end

  def trace_seed(item, maps) do
    case maps do
      [] -> item
      [map | maps] -> trace_seed(lookup(map, item), maps)
    end
  end

  # Part 2:

  def find_seed_location_intervals(input_file) do
    [seed_list | maps_in] = read_input(input_file)

    seeds =
      Regex.scan(~r/(\d+)/, seed_list, capture: :all_but_first)
      |> List.flatten()
      |> Enum.chunk_every(2)
      |> Enum.map(fn [s, l] ->
        s = String.to_integer(s)
        l = String.to_integer(l)
        {s, s + l - 1}
      end)

    maps = parse_maps(maps_in)

    maps
    |> List.foldl(seeds, fn map, acc -> map_intervals(acc, map) end)
    |> Enum.min_by(fn r -> elem(r, 0) end)
    |> elem(0)
  end

  def map_intervals(intervals, map, result \\ {[], []}) do
    [{map_interval, _dst} | r_map_items] = map
    {mapped, unmapped} = result

    case intervals do
      [] ->
        case r_map_items do
          [] ->
            mapped ++ unmapped

          _ ->
            map_intervals(unmapped, r_map_items, {mapped, []})
        end

      [interval | r_intervals] ->
        {matched, unmatched} = div_interval(interval, map_interval)

        new_matched =
          if is_nil(matched), do: mapped, else: [lookup_interval(map, matched) | mapped]

        new_result = {new_matched, unmapped ++ unmatched}
        map_intervals(r_intervals, map, new_result)
    end
  end

  def div_interval({s1, e1}, {s2, e2}) do
    cond do
      e1 < s2 || s1 > e2 -> {nil, [{s1, e1}]}
      s1 >= s2 && e1 <= e2 -> {{s1, e1}, []}
      s1 < s2 && e1 <= e2 -> {{s2, e1}, [{s1, s2 - 1}]}
      s1 >= s2 && e1 > e2 -> {{s1, e2}, [{e2 + 1, e1}]}
      true -> {{s2, e2}, [{s1, s2 - 1}, {e2 + 1, e1}]}
    end
  end

  # Common Code

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
            parse_maps(rest, {maps, [{{src, src + len - 1}, dst} | cur_map]})
        end
    end
  end

  def lookup_interval(map, {s, e}) do
    {lookup(map, s), lookup(map, e)}
  end

  def lookup(map, item) do
    map_item = map |> Enum.find(nil, fn {{s, e}, _d} -> item >= s && item <= e end)

    case map_item do
      nil -> item
      {{src, _}, dst} -> dst + item - src
    end
  end
end
