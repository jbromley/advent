defmodule Hand do
  defstruct ~w[cards type bid]a

  def greater?(h1, h2) do
    cond do
      h1.type < h2.type -> true
      h1.type > h2.type -> false
      true -> h1.cards <= h2.cards
    end
  end
end

defmodule CamelCards do
  @moduledoc """
  Solution for Advent of Code 2023, day 7 - Camel Cards.
  """
  @five_of_a_kind 7
  @four_of_a_kind 6
  @full_house 5
  @three_of_a_kind 4
  @two_pair 3
  @one_pair 2
  @high_card 1

  # Part 1

  @card_values %{
    "T" => "A",
    "J" => "B",
    "Q" => "C",
    "K" => "D",
    "A" => "E"
  }

  def total_winnings(input_file) do
    stream_input(input_file)
    |> Enum.sort(&Hand.greater?/2)
    |> Enum.with_index(fn hand, index -> (index + 1) * hand.bid end)
    |> Enum.sum()
  end

  # Part 2

  @card_values_jokers %{
    "T" => "A",
    "J" => "1",
    "Q" => "C",
    "K" => "D",
    "A" => "E"
  }

  def total_winnings_with_joker(input_file) do
    stream_input(input_file)
    |> Stream.map(&use_joker/1)
  end

  def use_joker(hand) do
  end

  # Common code

  def stream_input(input_file) do
    File.stream!(input_file)
    |> Stream.map(&String.split(&1, " ", trim: true))
    |> Stream.map(fn [hand, bid] ->
      c =
        String.replace(hand, ["T", "J", "Q", "K", "A"], fn match ->
          Map.get(@card_values, match)
        end)

      t = find_type(c)

      b = String.to_integer(String.trim(bid))
      %Hand{cards: c, type: t, bid: b}
    end)
  end

  def find_type(hand) do
    freqs = hand |> String.to_charlist() |> Enum.frequencies() |> Map.values() |> Enum.sort()

    case freqs do
      [5] -> @five_of_a_kind
      [1, 4] -> @four_of_a_kind
      [2, 3] -> @full_house
      [1, 1, 3] -> @three_of_a_kind
      [1, 2, 2] -> @two_pair
      [1, 1, 1, 2] -> @one_pair
      _ -> @high_card
    end
  end
end
