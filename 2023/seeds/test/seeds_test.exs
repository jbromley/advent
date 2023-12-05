defmodule SeedsTest do
  use ExUnit.Case
  doctest Seeds

  test "test finding the soil for each seed" do
    assert Seeds.find_seed_soil("input1") == 35
  end
end
