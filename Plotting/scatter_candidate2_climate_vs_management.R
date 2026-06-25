# ============================================================
# ActNow scatter plot - Candidate 2
# Climate impact vs management effect
#
# Reference-aware version:
# - climate impact is read from relative_historical_climate OR
#   relative_control_climate products
# - management effect is read from relative_climate_intervention
# - scenario variants such as rr-cw / rr-hw are paired against their
#   parent climate-only scenario through scenario_family
# ============================================================

library(tidyverse)
library(forcats)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::lag)

# ------------------------------------------------------------
# User settings
# ------------------------------------------------------------

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"
PLOT_TYPE <- "scatter_climate_impact_vs_management_effect"

INDICATOR_FILTER <- "mean_trophic_level"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

# Products required for this plot.
#
# x-axis:
#   relative_historical_climate OR relative_control_climate
#
# y-axis:
#   relative_climate_intervention
#
# This deliberately avoids the deprecated products:
#   relative_historical
#   relative_intervention
PRODUCT_FILTER <- paste(
  c(
    "relative_historical_climate",
    "relative_control_climate",
    "relative_climate_intervention"
  ),
  collapse = "|"
)

# ------------------------------------------------------------
# Shared utilities
# ------------------------------------------------------------

if (!file.exists(UTILS_FILE)) {
  stop("Cannot find utility file: ", UTILS_FILE)
}

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

if (nrow(actnow_data) == 0) {
  stop("No data rows loaded from matching files.")
}

final_values <- select_final_actnow_values(actnow_data)

# ------------------------------------------------------------
# Build reference-aware paired data
# ------------------------------------------------------------

# Climate-only relative change against the relevant ActNow reference.
# For most MEMs this is historical; for rCaN this may be control.
climate_df <- final_values |>
  dplyr::filter(
    .data$product %in% c(
      "relative_historical_climate",
      "relative_control_climate"
    ),
    .data$indicator == INDICATOR_FILTER
  ) |>
  dplyr::mutate(
    reference_type = dplyr::case_when(
      "reference_type" %in% names(final_values) ~ as.character(.data$reference_type),
      .data$product == "relative_historical_climate" ~ "historical",
      .data$product == "relative_control_climate" ~ "control",
      TRUE ~ NA_character_
    ),
    # Pair scenario variants (e.g. rr-cw, rr-hw) with their parent
    # climate-only scenario using metadata$scenarios$parent, exposed as
    # scenario_family by actnow_data_utils.
    climate_scenario = dplyr::coalesce(.data$scenario_family, .data$scenario)
  ) |>
  dplyr::select(
    model_id,
    climate_scenario,
    indicator,
    reference_type,
    climate_product = product,
    climate_only = value
  )

# Management effect relative to the corresponding climate-only run.
management_df <- final_values |>
  dplyr::filter(
    .data$product == "relative_climate_intervention",
    .data$indicator == INDICATOR_FILTER
  ) |>
  dplyr::mutate(
    climate_scenario = dplyr::coalesce(.data$scenario_family, .data$scenario)
  ) |>
  dplyr::select(
    model_id,
    model_abbreviation,
    model_label,
    case_study_id,
    scenario,
    scenario_family,
    climate_scenario,
    scenario_label,
    scenario_order,
    indicator,
    indicator_label,
    management_product = product,
    management_effect = value
  )

paired_df <- management_df |>
  dplyr::left_join(
    climate_df,
    by = c("model_id", "climate_scenario", "indicator")
  ) |>
  dplyr::filter(
    !is.na(.data$climate_only),
    !is.na(.data$management_effect)
  ) |>
  dplyr::mutate(
    basin_group = dplyr::case_when(
      .data$case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      .data$case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      .data$case_study_id %in% c("cs07") ~ "Black Sea",
      .data$case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ .data$case_study_id
    ),
    reference_label = dplyr::case_when(
      .data$reference_type == "historical" ~ "Historical reference",
      .data$reference_type == "control" ~ "Control reference",
      TRUE ~ "Reference"
    ),
    point_label = paste(.data$model_abbreviation, .data$scenario, sep = " - ")
  )

if (nrow(paired_df) == 0) {
  stop("No complete reference-aware paired rows found.")
}

duplicate_pairs <- paired_df |>
  dplyr::count(
    .data$model_id,
    .data$scenario,
    .data$indicator,
    name = "n"
  ) |>
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_pairs) > 0) {
  print(duplicate_pairs)
  stop("Duplicated paired rows detected.")
}

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

x_limit <- max(abs(paired_df$climate_only), na.rm = TRUE)
y_limit <- max(abs(paired_df$management_effect), na.rm = TRUE)

x_limit <- ceiling(x_limit / 10) * 10
y_limit <- ceiling(y_limit / 10) * 10

if (x_limit == 0) {
  x_limit <- 10
}

if (y_limit == 0) {
  y_limit <- 10
}

plot_title <- paste0(
  unique(na.omit(paired_df$indicator_label))[1],
  " - climate impact versus management effect"
)

reference_types <- paired_df |>
  dplyr::distinct(.data$reference_label) |>
  dplyr::pull(.data$reference_label) |>
  sort()

plot_subtitle <- paste0(
  "X-axis uses climate-only change relative to each model's reference; ",
  "Y-axis uses climate+management relative to climate-only. ",
  "References present: ",
  paste(reference_types, collapse = ", "),
  "."
)

p <- ggplot(
  paired_df,
  aes(
    x = climate_only,
    y = management_effect,
    colour = basin_group,
    shape = scenario
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_point(
    size = 3.4,
    stroke = 1.1
  ) +
  ggrepel::geom_text_repel(
    aes(label = model_abbreviation),
    size = 3,
    max.overlaps = 30,
    show.legend = FALSE
  ) +
  coord_cartesian(
    xlim = c(-x_limit, x_limit),
    ylim = c(-y_limit, y_limit)
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Climate-only final state relative to historical/control reference (%)",
    y = "Management effect relative to climate-only outcome (%)",
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

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

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
