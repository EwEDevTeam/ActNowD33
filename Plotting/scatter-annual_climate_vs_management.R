# ============================================================
# ActNow annual point cloud
# Climate-only vs climate+management, annual points
# ============================================================

library(tidyverse)
library(forcats)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::lag)

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"

PLOT_TYPE <- "annual_climate_vs_management_cloud"

INDICATOR_FILTER <- "total_biomass"
PRODUCT_FILTER <- "relative_historical|relative_intervention"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

START_YEAR <- 2030
END_YEAR <- 2050

FACET_BY <- "basin_group"   # "basin_group" or "scenario_label"

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

plot_data <- actnow_data |>
  dplyr::mutate(
    year = as.integer(format(.data$date, "%Y"))
  ) |>
  dplyr::filter(
    .data$year >= START_YEAR,
    .data$year <= END_YEAR,
    .data$product %in% c("relative_historical", "relative_intervention"),
    .data$indicator == INDICATOR_FILTER
  )

# For monthly data, reduce to annual mean.
annual_data <- plot_data |>
  dplyr::group_by(
    .data$model_id,
    .data$model_abbreviation,
    .data$model_label,
    .data$case_study_id,
    .data$scenario,
    .data$scenario_label,
    .data$scenario_order,
    .data$indicator,
    .data$indicator_label,
    .data$product,
    .data$year
  ) |>
  dplyr::summarise(
    value = mean(.data$value, na.rm = TRUE),
    n_dates = dplyr::n_distinct(.data$date),
    .groups = "drop"
  )

paired_df <- annual_data |>
  tidyr::pivot_wider(
    names_from = product,
    values_from = value
  ) |>
  dplyr::filter(
    !is.na(.data$relative_historical),
    !is.na(.data$relative_intervention)
  ) |>
  dplyr::mutate(
    climate_management = .data$relative_historical,
    climate_only = .data$relative_historical - .data$relative_intervention,
    management_effect = .data$climate_management - .data$climate_only,
    
    basin_group = dplyr::case_when(
      .data$case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      .data$case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      .data$case_study_id %in% c("cs07") ~ "Black Sea",
      .data$case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ .data$case_study_id
    ),
    
    year_factor = factor(.data$year),
    line_id = paste(.data$model_abbreviation, .data$scenario, sep = " - ")
  )

if (nrow(paired_df) == 0) {
  stop("No paired annual rows found.")
}

duplicate_rows <- paired_df |>
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
  stop("Duplicate annual paired rows detected.")
}

cat("\nAnnual paired rows:\n")
print(
  paired_df |>
    dplyr::count(.data$basin_group, .data$model_abbreviation, .data$scenario, name = "n_years") |>
    dplyr::arrange(.data$basin_group, .data$model_abbreviation, .data$scenario),
  n = Inf
)
cat("\n")

axis_limit <- max(
  abs(c(paired_df$climate_only, paired_df$climate_management)),
  na.rm = TRUE
)
axis_limit <- ceiling(axis_limit / 10) * 10

facet_formula <- if (FACET_BY == "scenario_label") {
  stats::as.formula("scenario_label ~ .")
} else {
  stats::as.formula("basin_group ~ .")
}

plot_title <- paste0(
  unique(na.omit(paired_df$indicator_label))[1],
  " - annual climate-only vs climate+management point cloud"
)

p <- ggplot(
  paired_df,
  aes(
    x = climate_only,
    y = climate_management,
    colour = year,
    group = line_id
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey45"
  ) +
  geom_path(
    alpha = 0.15,
    linewidth = 0.6
  ) +
  geom_point(
    aes(shape = model_abbreviation),
    size = 2.8,
    alpha = 0.8,
    stroke = 0.8
  ) +
  facet_grid(
    facet_formula,
    scales = "free",
    space = "free",
    switch = "y"
  ) +
  coord_equal(
    xlim = c(-axis_limit, axis_limit),
    ylim = c(-axis_limit, axis_limit)
  ) +
  scale_colour_viridis_c(
    name = "Year"
  ) +
  labs(
    title = plot_title,
    subtitle = paste0("Annual means, ", START_YEAR, "-", END_YEAR, ". Dashed line marks equal climate-only and climate+management outcomes."),
    x = "Climate-only state relative to historical baseline (%)",
    y = "Climate+management state relative to historical baseline (%)",
    colour = "Year",
    shape = "Model"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 90, size = 10),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    panel.spacing.y = unit(0.8, "lines"),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

OUTPUT_FILE <- make_actnow_plot_filename(
  plot_type = PLOT_TYPE,
  data = paired_df
)

DATA_FILE <- sub("\\.png$", ".csv", OUTPUT_FILE)

ggsave(
  filename = OUTPUT_FILE,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

readr::write_csv(
  paired_df,
  DATA_FILE
)

cat("Saved plot to: ", OUTPUT_FILE, "\n", sep = "")
cat("Saved plot data to: ", DATA_FILE, "\n", sep = "")