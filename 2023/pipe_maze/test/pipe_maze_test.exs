defmodule PipeMazeTest do
  use ExUnit.Case
  doctest PipeMaze

  test "find_farthest on smallest example" do
    assert PipeMaze.find_farthest("input1") == 4
  end

  test "find_farthest on small example" do
    assert PipeMaze.find_farthest("input2") == 8
  end
end
