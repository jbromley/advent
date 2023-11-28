defmodule NoSpace do
  @moduledoc """
  Solution for No Space problem of Advent of Code 2022.
  """
  def read_input(input_file) do
    input_file |> File.read!() |> String.split("\n") |> Enum.drop(-1)
  end
end

defmodule FSNode do
  defstruct id: nil, name: "", size: 0, parent: nil, children: []
end

defmodule FileSystem do
  use Agent

  defstruct nodes: %{}, next_id: 0

  def start_link(_opts) do
    Agent.start_link(fn -> %FileSystem{} end)
  end

  def create_root(fs) do
    Agent.get_and_update(fs, &create_node(&1, "/"))
  end

  def add_child(fs, parent, name, size \\ 0) do
    Agent.get_and_update(fs, &create_child(&1, parent, name, size))
  end

  # Private helper functions
  defp create_node(%{next_id: id, nodes: nodes} = state, name, size \\ 0) do
    new_node = %FSNode{id: id, name: name, size: size}
    {new_node, %{state | nodes: Map.put(nodes, id, new_node), next_id: id + 1}}
  end

  defp create_child(
         %{next_id: id, nodes: nodes} = state,
         %{parent: parent_id} = parent,
         name,
         size \\ 0
       ) do
    child = %FSNode{id: id, name: name, size: size, parent: parent_id}
    {child, %{state | nodes: nodes, next_id: id + 1}}
  end
end
