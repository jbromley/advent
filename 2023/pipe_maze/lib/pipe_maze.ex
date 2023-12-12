defmodule PipeMap do
  def new(lines) do
    rows = Enum.count(lines)

    map = :array.from_list(lines)
    :array.resize(rows, map)
    :array.fix(map)
    map
  end

  def get(_map, {row, col}) when row < 0 or col < 0 do
    nil
  end

  def get(map, {row, col}) do
    String.at(:array.get(row, map), col)
  end

  def replace_start(map, {r, _c}, symbol) do
    new_row = String.replace(:array.get(r, map), ~r/S/, symbol)
    :array.set(r, new_row, map)
  end

  def find_start(map) do
    start = do_find_start(map, 0)
    directions = do_find_start_directions(map, start)
    {start, directions}
  end

  def to_list(map) do
    :array.sparse_to_list(map)
  end

  def add({r1, c1}, {r2, c2}) do
    {r1 + r2, c1 + c2}
  end

  def move({r, c}, :up), do: {r - 1, c}
  def move({r, c}, :right), do: {r, c + 1}
  def move({r, c}, :down), do: {r + 1, c}
  def move({r, c}, :left), do: {r, c - 1}

  defp do_find_start(map, row) do
    line = :array.get(row, map)

    case Regex.run(~r/S/, line, return: :index) do
      nil -> do_find_start(map, row + 1)
      [{col, _}] -> {row, col}
    end
  end

  defp do_find_start_directions(map, start) do
    [
      {{-1, 0}, "|7F", :up},
      {{0, 1}, "-7J", :right},
      {{1, 0}, "|JL", :down},
      {{0, -1}, "-LF", :left}
    ]
    |> List.foldl([], fn {move, symbols, dir}, acc ->
      symbol = get(map, add(start, move))
      if not is_nil(symbol) && String.contains?(symbols, symbol), do: [dir | acc], else: acc
    end)
  end
end

defmodule PipeMaze do
  @moduledoc """
  Solution for Advent of Code 2023, day 10 - Pipe Maze.
  """
  alias PipeMap, as: M

  @start_pipe %{
    [:down, :up] => "|",
    [:left, :right] => "-",
    [:down, :left] => "7",
    [:down, :right] => "F",
    [:left, :up] => "J",
    [:right, :up] => "L"
  }

  @directions %{
    {"|", :up} => :up,
    {"|", :down} => :down,
    {"-", :left} => :left,
    {"-", :right} => :right,
    {"7", :right} => :down,
    {"7", :up} => :left,
    {"F", :left} => :down,
    {"F", :up} => :right,
    {"J", :right} => :up,
    {"J", :down} => :left,
    {"L", :left} => :up,
    {"L", :down} => :right
  }

  # Part 1

  def find_farthest(input_file) do
    read_input(input_file) |> search_pipe()
  end

  def search_pipe(map) do
    {start, [dir1, _dir2]} = PipeMap.find_start(map)
    search_pipe(map, {M.move(start, dir1), dir1}, 1)
  end

  def search_pipe(map, {loc, dir}, distance) do
    case M.get(map, loc) do
      "S" ->
        div(distance, 2)

      symbol ->
        next_dir = Map.get(@directions, {symbol, dir})
        search_pipe(map, {M.move(loc, next_dir), next_dir}, distance + 1)
    end
  end

  # Part 2

  def measure_enclosed(input_file) do
    read_input(input_file) |> count_tiles()
  end

  def mark_pipes(map) do
    {start, [dir1, dir2]} = PipeMap.find_start(map)
    tube = mark_pipes(map, {M.move(start, dir1), dir1}, MapSet.new([start]))
    map = M.replace_start(map, start, Map.get(@start_pipe, [dir1, dir2]))
    {tube, map}
  end

  defp mark_pipes(map, {loc, dir}, tube) do
    case M.get(map, loc) do
      "S" ->
        tube

      symbol ->
        next_dir = Map.get(@directions, {symbol, dir})
        mark_pipes(map, {M.move(loc, next_dir), next_dir}, MapSet.put(tube, loc))
    end
  end

  def count_tiles(map) do
    {tube, map} = mark_pipes(map)

    M.to_list(map)
    |> Enum.with_index()
    |> Enum.map(fn {row, row_num} -> count_row_tiles(row_num, row, tube) end)
    |> Enum.sum()
  end

  def count_row_tiles(row_num, row, tube) do
    re = ~r/(\||F-*J|L-*7)/

    Regex.scan(re, row, capture: :all_but_first, return: :index)
    |> List.flatten()
    |> Enum.map(fn {s, l} -> {s, s + l - 1} end)
    |> Enum.filter(fn {s, e} ->
      MapSet.member?(tube, {row_num, s}) && MapSet.member?(tube, {row_num, e})
    end)
    |> Enum.chunk_every(2, 2, :discard)
    |> Enum.map(fn [{_, e1}, {s2, _}] ->
      (e1 + 1)..(s2 - 1)
      |> Enum.filter(fn col -> not MapSet.member?(tube, {row_num, col}) end)
      |> Enum.count()
    end)
    |> Enum.sum()
  end

  # Common code

  def read_input(input_file) do
    map_lines =
      File.stream!(input_file)
      |> Stream.map(&String.trim/1)
      |> Enum.to_list()

    PipeMap.new(map_lines)
  end
end
