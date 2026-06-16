# ============================================================
# ActNow scatter plot - Candidate 1
# Climate-only final state vs climate+management final state
# ============================================================

library(tidyverse)
library(forcats)
# library(conflicted)

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"
PLOT_TYPE <- "scatter_climate_vs_management"

INDICATOR_FILTER <- "total_biomass"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL
PRODUCT_FILTER <- "relative_historical|relative_intervention"

source(UTILS_FILE)

metadata <- read_actnow_metadata(METADATA_FILE)

files <- discover_actnow_files(
  data_root = DATA_ROOT,
  metadata = metadata,
  case_study_filter = CASE_STUDY_FILTER,
  scenario_filter = SCENARIO_FILTER,
  product_filter = PRODUCT_FILTER,
  indicator_filter = INDICATOR_FILTER
)

if (nrow(files) == 0) {
  stop("No matching ActNow files found.")
}

actnow_data <- load_actnow_data(files)
final_values <- select_final_actnow_values(actnow_data)

paired_df <- final_values |>
  dplyr::filter(
    product %in% c("relative_historical", "relative_intervention"),
    indicator == INDICATOR_FILTER
  ) |>
  select(
    model_id,
    model_abbreviation,
    model_label,
    case_study_id,
    scenario,
    scenario_label,
    scenario_order,
    indicator,
    indicator_label,
    product,
    value
  ) |>
  pivot_wider(
    names_from = product,
    values_from = value
  ) |>
  dplyr::filter(
    !is.na(relative_historical),
    !is.na(relative_intervention)
  ) |>
  mutate(
    climate_management = relative_historical,
    climate_only = relative_historical - relative_intervention,
    
    basin_group = case_when(
      case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      case_study_id %in% c("cs07") ~ "Black Sea",
      case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ case_study_id
    ),
    
    point_label = paste(model_abbreviation, scenario, sep = " - ")
  )

if (nrow(paired_df) == 0) {
  stop("No complete paired rows found.")
}

duplicate_pairs <- paired_df |>
  count(model_id, scenario, indicator, name = "n") |>
  dplyr::filter(n > 1)

if (nrow(duplicate_pairs) > 0) {
  print(duplicate_pairs)
  stop("Duplicated paired rows detected.")
}

axis_limit <- max(
  abs(c(paired_df$climate_only, paired_df$climate_management)),
  na.rm = TRUE
)

axis_limit <- ceiling(axis_limit / 10) * 10

plot_title <- paste0(
  unique(na.omit(paired_df$indicator_label))[1],
  " - climate-only vs climate+management final state"
)

p <- ggplot(
  paired_df,
  aes(
    x = climate_only,
    y = climate_management,
    colour = basin_group,
    shape = scenario
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey45"
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey75"
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    colour = "grey75"
  ) +
  geom_point(
    size = 3.2,
    stroke = 1.1
  ) +
  ggrepel::geom_text_repel(
    aes(label = model_abbreviation),
    size = 3,
    max.overlaps = 30,
    show.legend = FALSE
  ) +
  coord_equal(
    xlim = c(-axis_limit, axis_limit),
    ylim = c(-axis_limit, axis_limit)
  ) +
  labs(
    title = plot_title,
    subtitle = "Dashed line marks equal climate-only and climate+management outcomes",
    x = "Climate-only final state relative to historical baseline (%)",
    y = "Climate+management final state relative to historical baseline (%)",
    colour = "Basin",
    shape = "Scenario"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

OUTPUT_FILE <- make_actnow_plot_filename(
  plot_type = PLOT_TYPE,
  data = paired_df
)

ggsave(
  filename = OUTPUT_FILE,
  plot = p,
  width = 9,
  height = 7,
  dpi = 300
)

cat("Saved plot to: ", OUTPUT_FILE, "\n", sep = "")