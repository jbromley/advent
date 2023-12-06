alias Seeds, as: S
  
Benchee.run(
  %{
    "find_seed_location" => fn -> S.find_seed_location("input") end,
    "find_seed_location_intervals" => fn -> S.find_seed_location_intervals("input") end,
  },
  formatters: [Benchee.Formatters.Console]
)
