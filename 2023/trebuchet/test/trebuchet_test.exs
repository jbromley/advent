defmodule TrebuchetTest do
  use ExUnit.Case
  doctest Trebuchet

  test "test part one with example data" do
    input = [
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]

    assert Trebuchet.calibration_code(
             input,
             &Trebuchet.extract_calibration_number(&1, parse_words: false)
           ) == 142
  end

  test "test part two with example data" do
    input = [
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    ]

    assert Trebuchet.calibration_code(
             input,
             &Trebuchet.extract_calibration_number(&1, parse_words: true)
           ) == 281
  end
end
