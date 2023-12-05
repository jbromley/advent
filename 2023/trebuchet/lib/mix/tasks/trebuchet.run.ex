defmodule Mix.Tasks.Trebuchet.Run do
  use Mix.Task
  alias Trebuchet, as: T

  @shortdoc "Run code to solve Trebuchet problems and print results"
  def run(_) do
    IO.puts("Part 1: #{T.calculate_code_1("input")}")
    IO.puts("Part 2: #{T.calculate_code_2("input")}")
  end
end
