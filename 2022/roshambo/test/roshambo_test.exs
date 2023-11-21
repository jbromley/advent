defmodule RoshamboTest do
  use ExUnit.Case
  doctest Roshambo

  test "greets the world" do
    assert Roshambo.hello() == :world
  end
end
