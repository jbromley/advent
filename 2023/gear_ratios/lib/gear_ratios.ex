defmodule GearRatios do
  @moduledoc """
  Solution for Advent of Code 2023 day 3 - Gear Ratios.
  """
  defstruct [:rows, :cols, :s]

  @type location() :: {non_neg_integer(), non_neg_integer()}
  @type number_info() :: {non_neg_integer(), non_neg_integer()}

  @all_symbols ~r/([^\d\.]+)/
  @gear_symbols ~r/(\*)/
  @number_symbols ~r/(\d+)/

  # Part 1 

  def solve_part_1(input_file) do
    read_input(input_file) |> sum_part_numbers()
  end

  @spec sum_part_numbers([String.t()]) :: non_neg_integer()
  def sum_part_numbers(schematic) do
    symbols = find_symbols(schematic, @all_symbols)
    numbers = find_numbers(schematic)
    sum_part_numbers(symbols, Map.to_list(numbers), 0)
  end

  def sum_part_numbers(symbols, numbers, acc) do
    case numbers do
      [] ->
        acc

      [{location, {len, value}} | rest] ->
        if MapSet.size(MapSet.intersection(symbols, hull(location, len))) == 0 do
          sum_part_numbers(symbols, rest, acc)
        else
          sum_part_numbers(symbols, rest, acc + value)
        end
    end
  end

  # Part 2

  def solve_part_2(input_file) do
    read_input(input_file) |> sum_gear_ratios()
  end

  @spec sum_gear_ratios([String.t()]) :: non_neg_integer()
  def sum_gear_ratios(schematic) do
    gears = find_symbols(schematic, @gear_symbols) |> MapSet.to_list()
    numbers = schematic |> find_numbers()
    number_locs = numbers |> create_number_map()
    sum_gear_ratios(gears, number_locs, numbers, 0)
  end

  @spec sum_gear_ratios(
          [location()],
          MapSet.t(location()),
          %{location() => number_info()},
          non_neg_integer()
        ) ::
          non_neg_integer()
  def sum_gear_ratios(gears, number_locs, numbers, acc) do
    case gears do
      [] ->
        acc

      [gear | rest] ->
        gear_product = gear_ratio(gear, number_locs, numbers)
        sum_gear_ratios(rest, number_locs, numbers, acc + gear_product)
    end
  end

  @spec create_number_map(%{location() => number_info()}) :: MapSet.t(location())
  def create_number_map(numbers) do
    numbers |> Map.to_list() |> create_number_map([]) |> MapSet.new()
  end

  def create_number_map(numbers, acc) do
    case numbers do
      [] ->
        acc

      [{{row, col}, {len, _value}} | rest] ->
        locations = col..(col + len - 1) |> Enum.map(&{row, &1})
        create_number_map(rest, acc ++ locations)
    end
  end

  @spec gear_ratio(location(), MapSet.t(location()), %{location() => non_neg_integer()}) ::
          non_neg_integer()
  def gear_ratio(gear, number_locs, numbers) do
    gear_numbers =
      MapSet.intersection(number_locs, hull(gear))
      |> MapSet.to_list()
      |> Enum.map(&canonicalize_number(&1, numbers))
      |> Enum.dedup()

    if Enum.count(gear_numbers) == 1 do
      0
    else
      gear_numbers |> Enum.map(fn loc -> elem(Map.get(numbers, loc), 1) end) |> Enum.product()
    end
  end

  @spec canonicalize_number(location(), %{location() => non_neg_integer()}) :: location()
  def canonicalize_number({row, col} = location, number_map) do
    if Map.has_key?(number_map, location) do
      location
    else
      canonicalize_number({row, col - 1}, number_map)
    end
  end

  # Common functions

  @spec read_input(String.t()) :: [String.t()]
  def read_input(input_file) do
    input_file |> File.read!() |> String.split("\n", trim: true)
  end

  @doc """
  Given a schematic, returns a `MapSet` containing the row and column
  coordinates of every symbol.
  """
  @spec find_symbols([String.t()], Regex.t()) :: MapSet.t(location())
  def find_symbols(schematic, which_symbols) do
    Enum.with_index(schematic)
    |> List.foldl([], fn data, symbols -> symbols ++ find_line_symbols(data, which_symbols) end)
    |> MapSet.new()
  end

  @spec find_line_symbols({String.t(), non_neg_integer()}, Regex.t()) :: [location()]
  def find_line_symbols({data, row}, which_symbols) do
    Regex.scan(which_symbols, data, return: :index, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(&{row, elem(&1, 0)})
  end

  @doc """
  Given a schematic, returns a `Map` that maps all number locations to the
  value of the number.
  """
  @spec find_numbers([String.t()]) :: %{location() => number_info()}
  def find_numbers(schematic) do
    Enum.with_index(schematic)
    |> List.foldl([], fn data, numbers -> numbers ++ find_line_numbers(data) end)
    |> Map.new()
  end

  @spec find_line_numbers({String.t(), non_neg_integer()}) :: [
          {location(), number_info()}
        ]
  def find_line_numbers({data, row}) do
    Regex.scan(@number_symbols, data, return: :index, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(
      &{{row, elem(&1, 0)},
       {elem(&1, 1), String.to_integer(String.slice(data, elem(&1, 0), elem(&1, 1)))}}
    )
  end

  @doc """
  Finds the hull of a number. The hull is the set of all locations that
  surround the number. The hull is returned as a `MapSet`.
  """
  @spec hull(location(), non_neg_integer()) :: MapSet.t({location()})
  def hull({row, col}, len \\ 1) do
    cols = (col - 1)..(col + len)

    ((cols |> Enum.map(&{row - 1, &1})) ++
       [{row, col - 1}, {row, col + len}] ++ (cols |> Enum.map(&{row + 1, &1})))
    |> MapSet.new()
  end
end
