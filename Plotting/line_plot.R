# ============================================================
# ActNow line plot - standardised data pipeline
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

PLOT_TYPE <- "line_plot"

INDICATOR_FILTER <- "richness_proxy"
PRODUCT_FILTER <- "relative_historical"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

START_YEAR <- 2030
END_YEAR <- 2050

AGGREGATE_TO_ANNUAL <- TRUE
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
    year = as.integer(format(.data$date, "%Y")),
    basin_group = dplyr::case_when(
      .data$case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      .data$case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      .data$case_study_id %in% c("cs07") ~ "Black Sea",
      .data$case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ .data$case_study_id
    ),
    line_id = paste(.data$model_abbreviation, .data$scenario, sep = " - ")
  ) |>
  dplyr::filter(
    .data$year >= START_YEAR,
    .data$year <= END_YEAR
  )

if (AGGREGATE_TO_ANNUAL) {
  plot_data <- plot_data |>
    dplyr::group_by(
      .data$model_id,
      .data$model_abbreviation,
      .data$model_label,
      .data$case_study_id,
      .data$basin_group,
      .data$product,
      .data$scenario,
      .data$scenario_label,
      .data$scenario_order,
      .data$indicator,
      .data$indicator_label,
      .data$line_id,
      .data$year
    ) |>
    dplyr::summarise(
      value = mean(.data$value, na.rm = TRUE),
      n_dates = dplyr::n_distinct(.data$date),
      .groups = "drop"
    )
} else {
  plot_data <- plot_data |>
    dplyr::mutate(
      year = as.numeric(.data$date)
    )
}

if (nrow(plot_data) == 0) {
  stop("No data rows available for selected line plot.")
}

facet_formula <- if (FACET_BY == "scenario_label") {
  stats::as.formula("scenario_label ~ .")
} else {
  stats::as.formula("basin_group ~ .")
}

plot_title <- paste0(
  unique(na.omit(plot_data$indicator_label))[1],
  " - line trajectories"
)

p <- ggplot(
  plot_data,
  aes(
    x = year,
    y = value,
    group = line_id,
    colour = scenario_label
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey70"
  ) +
  geom_line(
    linewidth = 0.75,
    alpha = 0.75
  ) +
  geom_point(
    aes(shape = model_abbreviation),
    size = 2.2,
    alpha = 0.85
  ) +
  facet_grid(
    facet_formula,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  labs(
    title = plot_title,
    subtitle = paste0(
      "Relative to historical baseline, ",
      START_YEAR,
      "-",
      END_YEAR
    ),
    x = "Year",
    y = "Change relative to historical baseline (%)",
    colour = "Scenario",
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
  data = plot_data
)

DATA_FILE <- sub("\\.png$", ".csv", OUTPUT_FILE)

ggsave(
  filename = OUTPUT_FILE,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

readr::write_csv(plot_data, DATA_FILE)

cat("Saved plot to: ", OUTPUT_FILE, "\n", sep = "")
cat("Saved plot data to: ", DATA_FILE, "\n", sep = "")