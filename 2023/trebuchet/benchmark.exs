alias Trebuchet, as: T
  
Benchee.run(
  %{
    "calculate_code_1" => fn -> T.calculate_code_1("input") end,
    "calculate_code_2" => fn -> T.calculate_code_2("input") end,
  },
  formatters: [Benchee.Formatters.Console]
)
