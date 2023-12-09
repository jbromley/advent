defmodule OasisTest do
  use ExUnit.Case
  doctest Oasis

  test "forecast all" do
    assert Oasis.forecast_all("input1") == 114
  end

  test "backcast all" do
    assert Oasis.backcast_all("input1") == 2
  end
end
