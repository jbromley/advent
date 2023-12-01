defmodule Trebuchet do
  @moduledoc """
  Day 1 of Advent of Code 2023 - Trebuchet?!
  """
  @number_re ~r/(zero|one|two|three|four|five|six|seven|eight|nine)/
  @numbers %{
    "zero" => "0",
    "one" => "1",
    "two" => "2",
    "three" => "3",
    "four" => "4",
    "five" => "5",
    "six" => "6",
    "seven" => "7",
    "eight" => "8",
    "nine" => "9"
  }

  def calculcate_code1(input_file) do
    input_file
    |> read_input()
    |> calibration_code1()
  end

  def calculcate_code2(input_file) do
    input_file
    |> read_input()
    |> calibration_code2()
  end

  def read_input(input_file) do
    File.read!(input_file)
    |> String.split("\n", trim: true)
  end

  @doc """
  Takes a list of strings and for each string extracts the calibration
  code and then sums all of the calibration codes. This version does *not*
  take digits written as words into account.
  """
  def calibration_code1(input) do
    input
    |> Enum.map(&extract_calibration/1)
    |> Enum.reduce(&(&1 + &2))
  end

  def calibration_code2(input) do
    input
    |> Enum.map(&extract_calibration(replace_number_words(&1)))
    |> Enum.reduce(&(&1 + &2))
  end

  @doc """
  Takes a string, extracts the first and last digit, and returns the
  resulting integer. Note that if there is only one digit in the string
  it *is* the first and last digit!
  """
  def extract_calibration(str) do
    value =
      Regex.scan(~r/(\d)(?:.*(\d))?/, str, capture: :all_but_first)
      |> List.flatten()
      |> Enum.join()
      |> String.to_integer()

    case value < 10 do
      true -> value * 11
      false -> value
    end
  end

  @doc """
  Takes a string and replaces digits written as words with the digit.
  """
  def replace_number_words(str) do
    String.replace(str, @number_re, fn word -> Map.get(@numbers, word) end)
  end
end
