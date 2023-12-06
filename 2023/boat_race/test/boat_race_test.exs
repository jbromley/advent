defmodule BoatRaceTest do
  use ExUnit.Case
  doctest BoatRace

  test "test counting wins" do
    assert BoatRace.count_all_wins("input1") == 288
  end

  test "test counting long race wins" do
    assert BoatRace.count_long_race_wins("input1") == 71503
  end
end
