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
    |> calibration_code(&extract_calibration_digits(&1))
  end

  def calculcate_code_2(input_file) do
    input_file
    |> read_input()
    |> calibration_code(&extract_calibration_digits_words(&1))
  end

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
  Extracts the calibration codes from a string considering only digits.
  """
  def extract_calibration_digits(str) do
    extract_calibration(str, @numbers_re)
  end

  @doc """
  Extracts the calibration codes from a string considering digits and
  numbers spelled out in words.
  """
  def extract_calibration_digits_words(str) do
    extract_calibration(str, @numbers_words_re)
  end

  defp extract_calibration(str, re) do
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
