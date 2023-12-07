defmodule CamelCardsTest do
  use ExUnit.Case
  doctest CamelCards

  test "total winnings" do
    assert CamelCards.total_winnings("input1") == 6440
  end

  test "total winnings with jokers" do
    assert CamelCards.total_winnings_with_jokers("input1") == 5905
  end
end
