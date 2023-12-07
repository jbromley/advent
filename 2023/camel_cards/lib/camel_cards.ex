defmodule Hand do
  @cards_no_joker ~w(2 3 4 5 6 7 8 9 A C D E)
  @five_of_a_kind 7
  @four_of_a_kind 6
  @full_house 5
  @three_of_a_kind 4
  @two_pair 3
  @one_pair 2
  @high_card 1

  defstruct ~w[cards type bid]a

  def new(cards, bid, use_joker \\ false) do
    cards =
      String.replace(cards, ["T", "J", "Q", "K", "A"], fn match ->
        Map.get(card_values(use_joker), match)
      end)

    hand = %Hand{cards: cards, type: type(cards), bid: bid}

    if use_joker, do: subst_joker(hand), else: hand
  end

  def compare(h1, h2) do
    cond do
      h1.type < h2.type ->
        :lt

      h1.type > h2.type ->
        :gt

      true ->
        cond do
          h1.cards < h2.cards -> :lt
          h1.cards > h2.cards -> :gt
          true -> :eq
        end
    end
  end

  def type(cards) do
    freqs = cards |> String.to_charlist() |> Enum.frequencies() |> Map.values() |> Enum.sort()

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

  def card_values(use_joker \\ false) do
    j_card = if not use_joker, do: "B", else: "1"

    %{
      "T" => "A",
      "J" => j_card,
      "Q" => "C",
      "K" => "D",
      "A" => "E"
    }
  end

  def subst_joker(hand) do
    joker_hand =
      @cards_no_joker
      |> Enum.map(fn c ->
        new_cards = String.replace(hand.cards, "1", c)
        %{hand | cards: new_cards, type: type(new_cards)}
      end)
      |> Enum.max(Hand)

    %{hand | type: joker_hand.type}
  end
end

defmodule CamelCards do
  @moduledoc """
  Solution for Advent of Code 2023, day 7 - Camel Cards.
  """

  def main(args) do
    if Enum.count(args) != 1 do
      IO.puts("Usage: camel_cards input_file")
      System.halt(1)
    end

    input_file = Enum.at(args, 0)

    IO.puts("Advent of Code 2023, day 7 - Camel Cards")
    IO.puts("Part 1: #{total_winnings(input_file)}")
    IO.puts("Part 2: #{total_winnings_with_jokers(input_file)}")
  end

  # Part 1

  def total_winnings(input_file, use_joker \\ false) do
    stream_input(input_file, use_joker)
    |> Enum.sort(Hand)
    |> Enum.with_index(fn hand, index -> (index + 1) * hand.bid end)
    |> Enum.sum()
  end

  # Part 2

  def total_winnings_with_jokers(input_file) do
    total_winnings(input_file, true)
  end

  # Common code

  def stream_input(input_file, use_joker \\ false) do
    File.stream!(input_file)
    |> Stream.map(&String.split(&1, " ", trim: true))
    |> Stream.map(fn [hand, bid] ->
      Hand.new(hand, String.to_integer(String.trim(bid)), use_joker)
    end)
  end
end
