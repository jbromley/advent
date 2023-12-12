defmodule PipeMazeTest do
  use ExUnit.Case
  doctest PipeMaze

  test "find_farthest on smallest example" do
    assert PipeMaze.find_farthest("input1") == 4
  end

  test "find_farthest on small example" do
    assert PipeMaze.find_farthest("input2") == 8
  end

  test "measure_enclosed test 1" do
    assert PipeMaze.measure_enclosed("input3") == 8
  end

  test "measure_enclosed test 2" do
    assert PipeMaze.measure_enclosed("input4") == 10
  end
end
