alias Wasteland, as: W
  
Benchee.run(
  %{
    "navigate_map" => fn -> W.navigate_map("input") end,
    "navigate_map_ghostlike" => fn -> W.navigate_map_ghostlike("input") end,
  },
  formatters: [Benchee.Formatters.Console]
)
