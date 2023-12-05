defmodule Trebuchet do
  @moduledoc """
  Day 1 of Advent of Code 2023 - Trebuchet?!
  """

  @numbers_re ~r/(\d)/
  @numbers_words_re ~r/(?=(\d|one|two|three|four|five|six|seven|eight|nine))/
  @numbers %{
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9
  }

  def main(args) do
    if Enum.empty?(args) do
      IO.puts("Usage: trebuchet input_file_file")
      System.halt(1)
    end

    input_file = Enum.at(args, 0)

    IO.puts("Day 1 - Trebuchet")
    IO.puts("Part 1: #{calculate_code_1(input_file)}")
    IO.puts("Part 2: #{calculate_code_2(input_file)}")
  end

  @doc """
  Calculates the calibration code for part 1 of the problem.
  """
  @spec calculate_code_1(String.t()) :: integer()
  def calculate_code_1(input_file) do
    input_file
    |> read_input()
    |> List.foldl(0, fn code, sum -> sum + extract_code(code, false) end)
  end

  @doc """
  Calculates the calibration code for part 2 of the problem.
  """
  @spec calculate_code_2(String.t()) :: integer()
  def calculate_code_2(input_file) do
    input_file
    |> read_input()
    |> List.foldl(0, fn code, sum -> sum + extract_code(code, true) end)
  end

  @doc """
  Reads the file named `input_file` and returns a list of strings.
  """
  @spec read_input(String.t()) :: list(String.t())
  def read_input(input_file) do
    File.read!(input_file)
    |> String.split("\n", trim: true)
  end

  @doc """
  Extracts the calibration number from a string. This means extracting
  the first and last number in the string and creating a number with those digits.
  Note that the first and last digit may be the same digit, i.e. there is only
  one digit in the string. If the option `parse_words` is `true` then any numbers
  spelled out as words are converted to digits before the extraction.
  """
  @spec extract_code(String.t(), boolean()) :: integer()
  def extract_code(str, parse_words \\ false) do
    re = if parse_words, do: @numbers_words_re, else: @numbers_re

    matches =
      Regex.scan(re, str, return: :index, capture: :all_but_first)
      |> List.flatten()

    first_digit = List.first(matches) |> match_to_digit(str)
    last_digit = List.last(matches) |> match_to_digit(str)

    10 * first_digit + last_digit
  end

  @spec match_to_digit({integer(), integer()}, String.t()) :: integer()
  defp match_to_digit({index, len}, str) do
    matched_str = String.slice(str, index, len)

    case len do
      1 -> matched_str |> String.to_integer()
      _ -> Map.get(@numbers, matched_str)
    end
  end
end
