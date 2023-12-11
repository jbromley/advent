alias Oasis, as: O
  
Benchee.run(
  %{
    "forecast_all" => fn -> O.forecast_all("input") end,
    "backcast_all" => fn -> O.backcast_all("input") end,
  },
  formatters: [Benchee.Formatters.Console]
)
