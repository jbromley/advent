defmodule WastelandTest do
  use ExUnit.Case
  doctest Wasteland

  test "navigate the map, no turn cycle" do
    assert Wasteland.navigate_map("input1") == 2
  end

  test "navigate the map, turn cycle" do
    assert Wasteland.navigate_map("input2") == 6
  end

  test "navigate the map like a ghost" do
    assert Wasteland.navigate_map_ghostlike("input3") == 6
  end
end
