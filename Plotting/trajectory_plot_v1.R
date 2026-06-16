library(tidyverse)

set.seed(42)

years <- 2025:2050

models <- tribble(
  ~basin_group, ~model_label, ~confidence_group,
  "Arctic", "BAL rCaN", "high",
  "Arctic", "BAL Atlantis", "high",
  "Arctic", "Fjord Ecosim", "low",
  "North Sea", "NS Ecospace", "high",
  "North Sea", "Wadden Sea Ecospace", "low",
  "Black Sea", "BAS Ecosim", "high",
  "Black Sea", "BAS Ecospace", "high",
  "Black Sea", "BUL Ecosim", "low",
  "Black Sea", "BUL Ecospace", "low",
  "Mediterranean", "Med OSMOSE", "high",
  "Mediterranean", "NWMed Ecospace", "high"
)

scenarios <- tribble(
  ~scenario_family, ~display_label, ~sort_order, ~target,
  "gs", "Global Sustainability", 1, 15,
  "in", "Inequality", 2, -5,
  "rr", "Regional Rivalry", 3, -18,
  "wm", "World Markets", 4, -28
)

trajectory_df <- crossing(
  models,
  scenarios,
  year = years
) |>
  group_by(model_label, scenario_family) |>
  mutate(
    model_offset = rnorm(1, 0, 5),
    noise = rnorm(n(), 0, 2),
    value = (year - min(year)) / (max(year) - min(year)) * target +
      model_offset +
      cumsum(noise) * 0.25
  ) |>
  ungroup()

p_spaghetti <- ggplot(
  trajectory_df,
  aes(
    x = year,
    y = value,
    colour = display_label,
    group = interaction(model_label, display_label)
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(aes(alpha = confidence_group), linewidth = 0.8) +
  facet_grid(basin_group ~ ., scales = "free_y") +
  scale_alpha_manual(values = c("high" = 0.95, "low" = 0.35)) +
  labs(
    title = "Total biomass trajectories",
    x = NULL,
    y = "Change relative to historical baseline (%)",
    colour = "Scenario",
    alpha = "Confidence"
  ) +
  theme_classic(base_size = 12)

print(p_spaghetti)