defmodule Trebuchet do
  @moduledoc """
  Day 1 of Advent of Code 2023 - Trebuchet?!
  """

  @digits_re ~r/\d/
  @digits %{
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9,
    "zero" => 0
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

  # Part 1

  @spec calculate_code_1(String.t()) :: non_neg_integer()
  def calculate_code_1(input_file) do
    input_file
    |> stream_input()
    |> Stream.map(fn line ->
      first = run_match(@digits_re, line, & &1)
      last = run_match(@digits_re, reverse(line), & &1)
      join_digits([first, last])
    end)
    |> Enum.sum()
  end

  # Part 2

  @spec calculate_code_2(String.t()) :: non_neg_integer()
  def calculate_code_2(input_file) do
    text_re = @digits |> Map.keys() |> Enum.join("|")
    forward_re = Regex.compile!("[[:digit:]]" <> "|" <> text_re)
    reverse_re = Regex.compile!("[[:digit:]]" <> "|" <> reverse(text_re))

    input_file
    |> stream_input()
    |> Stream.map(fn line ->
      first = run_match(forward_re, line, &Map.get(@digits, &1, &1))
      last = run_match(reverse_re, reverse(line), &Map.get(@digits, reverse(&1), &1))
      join_digits([first, last])
    end)
    |> Enum.sum()
  end

  # Common code

  @spec stream_input(String.t()) :: Stream.t()
  def stream_input(input_file) do
    input_file
    |> File.stream!()
    |> Stream.map(&String.trim(&1))
    |> Stream.filter(&(String.length(&1) > 0))
  end

  @spec run_match(Regex.t(), String.t(), (any() -> String.t())) :: String.t()
  defp run_match(re, str, fun) do
    [match] = Regex.run(re, str)
    fun.(match)
  end

  @spec join_digits([String.t()]) :: non_neg_integer()
  defp join_digits(digits) do
    digits |> Enum.join("") |> String.to_integer()
  end

  @spec reverse(String.t()) :: String.t()
  defp reverse(str) do
    String.reverse(str)
  end
end
