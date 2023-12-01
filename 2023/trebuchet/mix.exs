defmodule Trebuchet.MixProject do
  use Mix.Project

  def project do
    [
      app: :trebuchet,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: [main_module: Trebuchet],
      releases: [
        trebuchet: [
          steps: [:assemble, &Bakeware.assemble/1]
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Trebuchet, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:bakeware, "~> 0.2.4"}
    ]
  end
end
