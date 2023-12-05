defmodule TrebuchetTest do
  use ExUnit.Case
  doctest Trebuchet

  test "test part one with example data" do
    assert Trebuchet.calculate_code_1("input1") == 142
  end

  test "test part two with example data" do
    assert Trebuchet.calculate_code_2("input2") == 281
  end
end
