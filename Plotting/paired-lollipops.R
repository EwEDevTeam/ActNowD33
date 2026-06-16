# ============================================================
# ActNow paired lollipop plot - real data version
# ============================================================
# Purpose:
#   Plot paired climate-only and climate+management effects from the
#   standardised ActNow output folders.
#
# Data logic:
#   relative_historical   = climate+management final state relative to historical
#   relative_intervention = management effect relative to climate-only
#
# Therefore:
#   climate_management = relative_historical
#   climate_only       = relative_historical - relative_intervention
#
# This assumes both relative products are expressed as percentage-point
# changes on the same baseline.
# ============================================================

library(tidyverse)
library(forcats)

# ------------------------------------------------------------
# User settings
# ------------------------------------------------------------

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"
PLOT_TYPE = "paired_lollipop"

# Regex filters. Leave NULL to include all available data.
INDICATOR_FILTER <- "total_biomass"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL
PRODUCT_FILTER <- "relative_historical|relative_intervention"

# ------------------------------------------------------------
# Shared utilities
# ------------------------------------------------------------

if (!file.exists(UTILS_FILE)) {
  stop("Cannot find utility file: ", UTILS_FILE)
}

source(UTILS_FILE)

required_functions <- c(
  "read_actnow_metadata",
  "discover_actnow_files",
  "load_actnow_data",
  "select_final_actnow_values"
)

missing_functions <- required_functions[!vapply(required_functions, exists, logical(1))]
if (length(missing_functions) > 0) {
  stop(
    "The utility file is missing required functions: ",
    paste(missing_functions, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Metadata and file discovery
# ------------------------------------------------------------

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

if (exists("print_actnow_file_diagnostics")) {
  print_actnow_file_diagnostics(files)
}

# ------------------------------------------------------------
# Load final values
# ------------------------------------------------------------

actnow_data <- load_actnow_data(files)

if (nrow(actnow_data) == 0) {
  stop("No data rows loaded from matching files.")
}

if (exists("print_actnow_data_diagnostics")) {
  print_actnow_data_diagnostics(actnow_data)
}

final_values <- select_final_actnow_values(actnow_data)

# ------------------------------------------------------------
# Build paired data
# ------------------------------------------------------------

required_columns <- c(
  "model_id",
  "model_abbreviation",
  "model_label",
  "case_study_id",
  "scenario",
  "scenario_label",
  "scenario_family",
  "scenario_variant",
  "scenario_order",
  "indicator",
  "indicator_label",
  "product",
  "value"
)

missing_columns <- setdiff(required_columns, names(final_values))
if (length(missing_columns) > 0) {
  stop(
    "Loaded data is missing required columns for plotting: ",
    paste(missing_columns, collapse = ", ")
  )
}

paired_source <- final_values |>
  filter(
    product %in% c("relative_historical", "relative_intervention"),
    indicator == INDICATOR_FILTER
  ) |>
  select(
    all_of(required_columns)
  ) |>
  pivot_wider(
    names_from = product,
    values_from = value
  ) |>
  filter(
    !is.na(relative_historical),
    !is.na(relative_intervention)
  ) |>
  mutate(
    climate_management = relative_historical,
    climate_only = relative_historical - relative_intervention,

    # Temporary grouping until basin/case metadata is represented explicitly
    # in actnow_metadata.yaml.
    basin_group = case_when(
      case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      case_study_id %in% c("cs07") ~ "Black Sea",
      case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ case_study_id
    ),

    display_label = scenario_label,
    pair_id = paste(model_abbreviation, display_label, sep = " - ")
  )

if (nrow(paired_source) == 0) {
  stop(
    "No complete paired rows found. Need both relative_historical and ",
    "relative_intervention for the selected indicator."
  )
}

# Make sure each model/scenario/indicator gives one paired row.
duplicate_pairs <- paired_source |>
  count(model_id, scenario, indicator, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_pairs) > 0) {
  print(duplicate_pairs)
  stop("Duplicated paired rows detected.")
}

# ------------------------------------------------------------
# Factor ordering
# ------------------------------------------------------------

basin_levels <- c("Arctic", "North Sea", "Black Sea", "Mediterranean")

paired_df <- paired_source |>
  arrange(
    factor(basin_group, levels = basin_levels),
    model_abbreviation,
    scenario_order,
    scenario
  ) |>
  mutate(
    basin_group = factor(basin_group, levels = basin_levels),
    pair_id = factor(pair_id, levels = rev(unique(pair_id)))
  )

point_df <- paired_df |>
  select(
    indicator,
    indicator_label,
    basin_group,
    model_label,
    model_abbreviation,
    scenario,
    scenario_family,
    scenario_variant,
    scenario_label,
    scenario_order,
    display_label,
    pair_id,
    climate_only,
    climate_management
  ) |>
  pivot_longer(
    cols = c(climate_only, climate_management),
    names_to = "run_type",
    values_to = "value"
  ) |>
  mutate(
    basin_group = factor(basin_group, levels = basin_levels),
    pair_id = factor(pair_id, levels = levels(paired_df$pair_id)),
    run_type = factor(
      run_type,
      levels = c("climate_only", "climate_management")
    )
  )

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

cat("\nPaired rows used for plot:\n")
print(
  paired_df |>
    count(basin_group, model_abbreviation, scenario, name = "n") |>
    arrange(basin_group, model_abbreviation, scenario),
  n = Inf
)
cat("\n")

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

plot_title <- paste0(
  unique(na.omit(paired_df$indicator_label))[1],
  " - paired climate and management effects"
)

p <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey35"
  ) +

  # Thin grey line: historical baseline to climate-only.
  geom_segment(
    data = paired_df,
    aes(
      x = 0,
      xend = climate_only,
      y = pair_id,
      yend = pair_id
    ),
    colour = "grey75",
    linewidth = 0.55
  ) +

  # Thick coloured line: climate-only to climate+management.
  geom_segment(
    data = paired_df,
    aes(
      x = climate_only,
      xend = climate_management,
      y = pair_id,
      yend = pair_id,
      colour = display_label
    ),
    linewidth = 1.2
  ) +

  geom_point(
    data = point_df,
    aes(
      x = value,
      y = pair_id,
      colour = display_label,
      shape = run_type
    ),
    size = 3.1,
    stroke = 1.1
  ) +

  facet_grid(
    basin_group ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +

  scale_shape_manual(
    values = c(
      climate_only = 1,
      climate_management = 16
    ),
    labels = c(
      climate_only = "Climate only",
      climate_management = "Climate + management"
    )
  ) +

  labs(
    title = plot_title,
    x = "Percentage change relative to historical baseline (%)",
    y = NULL,
    colour = "Scenario",
    shape = "Run type"
  ) +

  theme_classic(base_size = 12) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90, size = 11),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    panel.spacing.y = unit(0.8, "lines"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
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
  width = 11,
  height = 8,
  dpi = 300
)

cat("Saved plot to: ", OUTPUT_FILE, "\n", sep = "")
