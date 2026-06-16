# ============================================================
# ActNow milestone trajectory plot
# 2030 / 2040 / 2050 comparison
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
PLOT_TYPE <- "milestone_trajectory"

INDICATOR_FILTER <- "total_biomass"
PRODUCT_FILTER <- "relative_historical"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

MILESTONE_YEARS <- c(2030, 2040, 2050)

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

# ------------------------------------------------------------
# Select nearest values to milestone dates
# ------------------------------------------------------------

milestones <- tibble::tibble(
  milestone_year = MILESTONE_YEARS,
  milestone_date = as.Date(paste0(MILESTONE_YEARS, "-07-01"))
)

milestone_df <- actnow_data |>
  dplyr::inner_join(
    milestones,
    by = character()
  ) |>
  dplyr::mutate(
    days_from_milestone = abs(as.numeric(.data$date - .data$milestone_date))
  ) |>
  dplyr::group_by(
    .data$model_id,
    .data$product,
    .data$scenario,
    .data$cadence,
    .data$indicator,
    .data$milestone_year
  ) |>
  dplyr::slice_min(
    order_by = .data$days_from_milestone,
    n = 1,
    with_ties = FALSE
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    year = .data$milestone_year,
    
    basin_group = dplyr::case_when(
      .data$case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      .data$case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      .data$case_study_id %in% c("cs07") ~ "Black Sea",
      .data$case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ .data$case_study_id
    ),
    
    line_id = paste(
      .data$model_abbreviation,
      .data$scenario,
      sep = " - "
    )
  )

if (nrow(milestone_df) == 0) {
  stop("No values found for requested milestone years.")
}

duplicate_rows <- milestone_df |>
  dplyr::count(
    .data$model_id,
    .data$scenario,
    .data$indicator,
    .data$year,
    name = "n"
  ) |>
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_rows) > 0) {
  print(duplicate_rows)
  stop("Duplicate milestone rows detected.")
}

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

cat("\nMilestone rows used for plot:\n")
print(
  milestone_df |>
    dplyr::count(
      .data$basin_group,
      .data$model_abbreviation,
      .data$scenario,
      .data$year,
      name = "n"
    ) |>
    dplyr::arrange(
      .data$basin_group,
      .data$model_abbreviation,
      .data$scenario,
      .data$year
    ),
  n = Inf
)
cat("\n")

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

plot_title <- paste0(
  unique(na.omit(milestone_df$indicator_label))[1],
  " - milestone trajectories"
)

basin_levels <- c("Arctic", "North Sea", "Black Sea", "Mediterranean")

milestone_df <- milestone_df |>
  dplyr::mutate(
    basin_group = factor(.data$basin_group, levels = basin_levels),
    year = factor(.data$year, levels = MILESTONE_YEARS)
  )

p <- ggplot(
  milestone_df,
  aes(
    x = year,
    y = value,
    group = line_id,
    colour = scenario
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey70"
  ) +
  geom_line(
    linewidth = 0.8,
    alpha = 0.8
  ) +
  geom_point(
    aes(shape = model_abbreviation),
    size = 2.8,
    stroke = 0.9
  ) +
  facet_grid(
    basin_group ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  labs(
    title = plot_title,
    subtitle = "Relative to historical baseline at 2030, 2040 and 2050",
    x = "Milestone year",
    y = "Change relative to historical baseline (%)",
    colour = "Scenario",
    shape = "Model"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90, size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.spacing.y = unit(0.8, "lines"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

OUTPUT_FILE <- make_actnow_plot_filename(
  plot_type = PLOT_TYPE,
  data = milestone_df
)

ggsave(
  filename = OUTPUT_FILE,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Saved plot to: ", OUTPUT_FILE, "\n", sep = "")