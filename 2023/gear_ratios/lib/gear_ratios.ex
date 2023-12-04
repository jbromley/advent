defmodule GearRatios do
  @moduledoc """
  Solution for Advent of Code 2023 day 3 - Gear Ratios.
  """
  defstruct [:rows, :cols, :s]

  @type location() :: {non_neg_integer(), non_neg_integer()}
  @type number_location() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}

  def solve_part_1(input_file) do
    read_input(input_file) |> sum_part_numbers()
  end

  @spec sum_part_numbers([String.t()]) :: non_neg_integer()
  def sum_part_numbers(schematic) do
    symbols = find_symbols(schematic)
    numbers = find_numbers(schematic)
    sum_part_numbers(symbols, Map.to_list(numbers), 0)
  end

  def sum_part_numbers(symbols, numbers, acc) do
    case numbers do
      [] ->
        acc

      [{number, value} | rest] ->
        if MapSet.size(MapSet.intersection(symbols, hull(number))) == 0 do
          sum_part_numbers(symbols, rest, acc)
        else
          sum_part_numbers(symbols, rest, acc + value)
        end
    end
  end

  @spec read_input(String.t()) :: [String.t()]
  def read_input(input_file) do
    input_file |> File.read!() |> String.split("\n", trim: true)
  end

  @doc """
  Given a schematic, returns a `MapSet` containing the row and column
  coordinates of every symbol.
  """
  @spec find_symbols([String.t()]) :: MapSet.t(location())
  def find_symbols(schematic) do
    Enum.with_index(schematic)
    |> List.foldl([], fn data, symbols -> symbols ++ find_line_symbols(data) end)
    |> MapSet.new()
  end

  @spec find_line_symbols({String.t(), non_neg_integer()}) :: [location()]
  def find_line_symbols({data, row}) do
    re = ~r/([^\d\.])/

    Regex.scan(re, data, return: :index, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(&{row, elem(&1, 0)})
  end

  @doc """
  Given a schematic, returns a `Map` that maps all number locations to the
  value of the number.
  """
  @spec find_numbers([String.t()]) :: %{location() => non_neg_integer()}
  def find_numbers(schematic) do
    Enum.with_index(schematic)
    |> List.foldl([], fn data, numbers -> numbers ++ find_line_numbers(data) end)
    |> Map.new()
  end

  @spec find_line_numbers({String.t(), non_neg_integer()}) :: [
          {number_location(), non_neg_integer()}
        ]
  def find_line_numbers({data, row}) do
    re = ~r/(\d+)/

    Regex.scan(re, data, return: :index, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(
      &{{row, elem(&1, 0), elem(&1, 1)},
       String.to_integer(String.slice(data, elem(&1, 0), elem(&1, 1)))}
    )
  end

  @doc """
  Finds the hull of a number. The hull is the set of all locations that
  surround the number. The hull is returned as a `MapSet`.
  """
  @spec hull(number_location()) :: MapSet.t({location()})
  def hull({row, col, len}) do
    cols = (col - 1)..(col + len)

    ((cols |> Enum.map(&{row - 1, &1})) ++
       [{row, col - 1}, {row, col + len}] ++ (cols |> Enum.map(&{row + 1, &1})))
    |> MapSet.new()
  end
end
