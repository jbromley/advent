defmodule PipeMap do

  def new(lines) do
    rows = Enum.count(lines)

    map = :array.from_list(lines)
    :array.resize(rows, map)
    :array.fix(map)
    map
  end

  def get(map, {row, col}) do
    String.at(:array.get(row, map), col)
  end

  def find_start(map) do
    start = do_find_start(map, 0)
    directions = do_find_start_directions(map, start)
    {start, directions}
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
    [{{-1, 0}, "|7F", :up}, {{0, 1}, "-7J", :right}, {{1, 0}, "|JL", :down}, {{0, -1}, "-LF", :left}]
    |> List.foldl([], fn {move, symbols, dir}, acc -> 
         if String.contains?(symbols, get(map, add(start, move))), do: [dir | acc], else: acc
       end)
  end
end

defmodule PipeMaze do
  @moduledoc """
  Solution for Advent of Code 2023, day 10 - Pipe Maze.
  """
  alias PipeMap, as: M

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
    {"L", :down} => :right,
  }

  # Part 1

  def find_farthest(input_file) do
    map = read_input(input_file)
    {start, [dir1, _dir2]} = PipeMap.find_start(map) 
    search(map, {M.move(start, dir1), dir1}, 1)
  end

  def search(map, {loc, dir}, distance) do
    case M.get(map, loc) do
      "S" -> div(distance, 2)
      symbol ->
        next_dir = Map.get(@directions, {symbol, dir}) 
        search(map, {M.move(loc, next_dir), next_dir}, distance + 1)
    end
  end
  
  #Part 2

  def measure_enclosed(input_file) do
    map = read_input(input_file)
    {start, [dir1, _dir2]} = PipeMap.find_start(map) 
    tube = MapSet.new()
    mark(map, {M.move(start, dir1), dir1}, MapSet.put(start))
  end

  def mark(map, {loc, dir}, tube \\ MapSet.new()) do
    case M.get(map, loc) do
      "S" -> 
        tube
      symbol -> 
        next_dir = Map.get(@directions, {symbol, dir})
        mark(map, {M.move(loc, next_dir), next_dir}, MapSet.put(loc))
    end
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
