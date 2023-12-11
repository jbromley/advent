defmodule PipeMazeTest do
  use ExUnit.Case
  doctest PipeMaze

  test "greets the world" do
    assert PipeMaze.hello() == :world
  end
end
