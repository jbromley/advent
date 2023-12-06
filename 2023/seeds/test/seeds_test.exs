defmodule SeedsTest do
  use ExUnit.Case
  doctest Seeds

  test "test finding the soil for each seed" do
    assert Seeds.find_seed_location("input1") == 35
  end

  test "test finding the soil for ranges of seeds" do
    assert Seeds.find_seed_location_intervals("input1") == 46
  end
end
