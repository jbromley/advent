defmodule Trebuchet do
  @moduledoc """
  Day 1 of Advent of Code 2023 - Trebuchet?!
  """
  @numbers_re ~r/(1|2|3|4|5|6|7|8|9)/
  @numbers_words_re ~r/(?=(1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine))/
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

  def calculcate_code_1(input_file) do
    input_file
    |> read_input()
    |> calibration_code(&extract_calibration_number(&1))
  end

  def calculcate_code_2(input_file) do
    input_file
    |> read_input()
    |> calibration_code(&extract_calibration_number(&1, parse_words: true))
  end

  @doc """
  Reads the file named `input_file` and returns a list of strings.
  """
  def read_input(input_file) do
    File.read!(input_file)
    |> String.split("\n", trim: true)
  end

  @doc """
  Takes a list of strings and an extraction function and for each string
  extracts the calibration code and then sums all of the calibration codes.
  """
  def calibration_code(input, extract_fn) do
    input
    |> Enum.map(&extract_fn.(&1))
    |> Enum.reduce(&(&1 + &2))
  end

  @doc """
  Extracts the calibration number from a string. This means extracting
  the first and last number in the string and creating a number with those digits.
  Note that the first and last digit may be the same digit, i.e. there is only
  one digit in the string. If the option `parse_words` is `true` then any numbers
  spelled out as words are converted to digits before the extraction.
  """
  def extract_calibration_number(str, opts \\ [parse_words: false]) do
    re = if Keyword.get(opts, :parse_words), do: @numbers_words_re, else: @numbers_re

    matches =
      Regex.scan(re, str, return: :index)
      |> List.flatten()
      |> Enum.filter(fn {_index, len} -> len != 0 end)

    first_digit = List.first(matches) |> (&match_to_digit(str, &1)).()
    last_digit = List.last(matches) |> (&match_to_digit(str, &1)).()

    10 * first_digit + last_digit
  end

  defp match_to_digit(str, {index, len}) do
    matched_str = String.slice(str, index, len)

    case len do
      1 -> matched_str |> String.to_integer()
      _ -> Map.get(@numbers, matched_str)
    end
  end
end
