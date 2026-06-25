# ============================================================
# ActNow line plot - standardised data pipeline
# ============================================================
#
# Notes:
#   - Uses the shared ActNow utilities for metadata, discovery and loading.
#   - Supports compact negative filters using "~regex".
#     Example: MODEL_FILTER <- "~^WAD-space$" excludes the WAD model.
#   - Avoids annual sawtooth artefacts by aggregating to one value per
#     model / product / scenario / indicator / year before plotting years.
#   - Product selection is exact and priority-based, so a broad regex such as
#     "relative_historical" does not accidentally pull in climate-only and
#     intervention products at the same time.
# ============================================================

library(tidyverse)
library(forcats)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::lag)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"

OUTPUT_DIR <- "."

source(UTILS_FILE)
metadata <- read_actnow_metadata(METADATA_FILE)

# ------------------------------------------------------------
# Plot settings
# ------------------------------------------------------------

PLOT_TYPE <- "line_plot"

INDICATOR_FILTER <- "richness_proxy"

# Model-specific relative products that represent the same conceptual output:
# climate + management change relative to the model's appropriate reference.
# For most models this is historical-reference output; for rCaN-style control
# simulations this may be control-reference output. Legacy relative_historical
# is kept as a fallback for older output folders.
PRODUCT_IDS <- c(
  "relative_historical_intervention",
  "relative_control_intervention",
  "relative_historical"
)

CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

# Compact negative filters are supported.
# Example: MODEL_FILTER <- "~^WAD-space$" excludes the WAD model.
MODEL_FILTER <- "~^WAD-space$"

CADENCE_FILTER <- NULL

START_YEAR <- 2030
END_YEAR <- 2050

AGGREGATE_TO_ANNUAL <- TRUE
FACET_BY <- "basin_group"   # "basin_group" or "scenario_label"

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

is_empty_filter <- function(pattern) {
  is.null(pattern) || length(pattern) == 0 || is.na(pattern) || pattern == ""
}

is_negative_filter <- function(pattern) {
  if (is_empty_filter(pattern)) {
    return(FALSE)
  }

  stringr::str_starts(as.character(pattern), "~")
}

clean_filter_pattern <- function(pattern) {
  if (is_negative_filter(pattern)) {
    return(stringr::str_sub(as.character(pattern), 2))
  }

  pattern
}

matches_actnow_filter <- function(x, pattern) {
  if (is_empty_filter(pattern)) {
    return(rep(TRUE, length(x)))
  }

  negative <- is_negative_filter(pattern)
  pattern <- clean_filter_pattern(pattern)

  matched <- stringr::str_detect(
    as.character(x),
    stringr::regex(pattern, ignore_case = TRUE)
  )

  matched <- tidyr::replace_na(matched, FALSE)

  if (negative) {
    return(!matched)
  }

  matched
}

get_column_or_na <- function(df, column_name) {
  if (column_name %in% names(df)) {
    return(df[[column_name]])
  }

  rep(NA_character_, nrow(df))
}

matches_any_field <- function(df, fields, pattern) {
  if (nrow(df) == 0) {
    return(logical())
  }

  if (is_empty_filter(pattern)) {
    return(rep(TRUE, nrow(df)))
  }

  if (is_negative_filter(pattern)) {
    keep <- rep(TRUE, nrow(df))

    for (field in fields) {
      keep <- keep & matches_actnow_filter(
        get_column_or_na(df, field),
        pattern
      )
    }

    return(keep)
  }

  keep <- rep(FALSE, nrow(df))

  for (field in fields) {
    keep <- keep | matches_actnow_filter(
      get_column_or_na(df, field),
      pattern
    )
  }

  keep
}

make_exact_id_regex <- function(ids) {
  ids <- ids[!is.na(ids)]
  ids <- ids[nchar(ids) > 0]

  if (length(ids) == 0) {
    stop("No product IDs were supplied.")
  }

  # Product IDs are ActNow identifiers consisting of letters, numbers and
  # underscores. Keep this intentionally simple and readable.
  paste0("^(", paste(ids, collapse = "|"), ")$")
}

label_or_id <- function(label, id) {
  out <- dplyr::coalesce(as.character(label), as.character(id))
  out[out == ""] <- as.character(id)[out == ""]
  out
}

# ------------------------------------------------------------
# Apply model filter before discovery
# ------------------------------------------------------------

model_fields <- c(
  "case_study_id",
  "model_id",
  "model_abbreviation",
  "model_label",
  "model_root"
)

keep_case_study <- matches_any_field(
  metadata$models,
  model_fields,
  CASE_STUDY_FILTER
)

keep_model <- matches_any_field(
  metadata$models,
  model_fields,
  MODEL_FILTER
)

candidate_models <- metadata$models[keep_case_study & keep_model, , drop = FALSE]

if (nrow(candidate_models) == 0) {
  stop("No models remain after applying CASE_STUDY_FILTER and MODEL_FILTER.")
}

excluded_models <- metadata$models |>
  dplyr::anti_join(
    candidate_models |> dplyr::select(.data$model_id),
    by = "model_id"
  )

if (nrow(excluded_models) > 0) {
  message(
    "Models excluded by filters:\n",
    paste0(
      "  - ",
      excluded_models$model_abbreviation,
      " / ",
      excluded_models$model_label,
      " [",
      excluded_models$model_id,
      "]",
      collapse = "\n"
    )
  )
}

metadata_for_plot <- metadata
metadata_for_plot$models <- candidate_models

# ------------------------------------------------------------
# Discover and load data
# ------------------------------------------------------------

product_filter <- make_exact_id_regex(PRODUCT_IDS)

files <- discover_actnow_files(
  data_root = DATA_ROOT,
  metadata = metadata_for_plot,
  case_study_filter = NULL,
  scenario_filter = SCENARIO_FILTER,
  product_filter = product_filter,
  indicator_filter = INDICATOR_FILTER,
  cadence_filter = CADENCE_FILTER,
  prefer_cadence = TRUE
)

if (nrow(files) == 0) {
  stop("No matching ActNow files found.")
}

# Prefer canonical product folders over legacy fallbacks where more than one
# product is available for the same model / scenario / indicator.
product_priority <- tibble::tibble(
  product = PRODUCT_IDS,
  product_priority = seq_along(PRODUCT_IDS)
)

files <- files |>
  dplyr::left_join(product_priority, by = "product") |>
  dplyr::mutate(
    product_priority = dplyr::coalesce(.data$product_priority, 9999L)
  ) |>
  dplyr::group_by(
    .data$model_id,
    .data$scenario,
    .data$indicator
  ) |>
  dplyr::arrange(
    .data$product_priority,
    .data$cadence_order,
    .by_group = TRUE
  ) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

message("Discovered files after product prioritisation:")
print(
  files |>
    dplyr::count(
      .data$product,
      .data$cadence,
      name = "n_files"
    ) |>
    dplyr::arrange(.data$product, .data$cadence)
)

file_diagnostics <- diagnose_actnow_file_index(files)
print_actnow_file_diagnostics(files)

actnow_data <- load_actnow_data(files)

if (nrow(actnow_data) == 0) {
  stop("Files were discovered, but no data rows were loaded.")
}

print_actnow_data_diagnostics(actnow_data)

# ------------------------------------------------------------
# Prepare plot data
# ------------------------------------------------------------

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
    model_label = label_or_id(.data$model_label, .data$model_id),
    model_abbreviation = label_or_id(.data$model_abbreviation, .data$model_id),
    scenario_label = label_or_id(.data$scenario_label, .data$scenario),
    indicator_label = label_or_id(.data$indicator_label, .data$indicator),
    product_label = label_or_id(.data$product_label, .data$product)
  ) |>
  dplyr::filter(
    !is.na(.data$date),
    !is.na(.data$value),
    .data$year >= START_YEAR,
    .data$year <= END_YEAR
  )

if (nrow(plot_data) == 0) {
  stop("No data rows available for selected line plot.")
}

if (AGGREGATE_TO_ANNUAL) {
  plot_data <- plot_data |>
    dplyr::group_by(
      .data$model_id,
      .data$model_abbreviation,
      .data$model_label,
      .data$model_order,
      .data$case_study_id,
      .data$basin_group,
      .data$product,
      .data$product_label,
      .data$product_order,
      .data$scenario,
      .data$scenario_label,
      .data$scenario_order,
      .data$indicator,
      .data$indicator_label,
      .data$indicator_order,
      .data$year
    ) |>
    dplyr::summarise(
      value = mean(.data$value, na.rm = TRUE),
      n_dates = dplyr::n_distinct(.data$date),
      start_date = min(.data$date, na.rm = TRUE),
      end_date = max(.data$date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x_value = .data$year
    )
} else {
  plot_data <- plot_data |>
    dplyr::mutate(
      x_value = .data$date,
      n_dates = 1L,
      start_date = .data$date,
      end_date = .data$date
    )
}

plot_data <- plot_data |>
  dplyr::mutate(
    line_id = interaction(
      .data$model_id,
      .data$product,
      .data$scenario,
      .data$indicator,
      drop = TRUE
    ),
    model_abbreviation = forcats::fct_reorder(
      .data$model_abbreviation,
      .data$model_order
    ),
    scenario_label = forcats::fct_reorder(
      .data$scenario_label,
      .data$scenario_order
    ),
    basin_group = factor(
      .data$basin_group,
      levels = c("Arctic", "North Sea", "Black Sea", "Mediterranean")
    )
  )

# ------------------------------------------------------------
# Diagnostics for line artefacts
# ------------------------------------------------------------

if (AGGREGATE_TO_ANNUAL) {
  duplicate_x_diagnostics <- plot_data |>
    dplyr::count(
      .data$line_id,
      .data$year,
      name = "n_values_per_line_year"
    ) |>
    dplyr::filter(.data$n_values_per_line_year > 1) |>
    dplyr::arrange(
      dplyr::desc(.data$n_values_per_line_year),
      .data$line_id,
      .data$year
    )
} else {
  duplicate_x_diagnostics <- plot_data |>
    dplyr::count(
      .data$line_id,
      .data$date,
      name = "n_values_per_line_date"
    ) |>
    dplyr::filter(.data$n_values_per_line_date > 1) |>
    dplyr::arrange(
      dplyr::desc(.data$n_values_per_line_date),
      .data$line_id,
      .data$date
    )
}

if (nrow(duplicate_x_diagnostics) > 0) {
  warning(
    "Line plot still has multiple y-values for the same line/x-position. ",
    "This can create vertical sawtooth artefacts. See printed diagnostics."
  )
  print(duplicate_x_diagnostics)
}

product_summary <- plot_data |>
  dplyr::distinct(.data$product, .data$product_label) |>
  dplyr::arrange(.data$product)

message("Products used in plot:")
print(product_summary)

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

facet_formula <- if (FACET_BY == "scenario_label") {
  stats::as.formula("scenario_label ~ .")
} else {
  stats::as.formula("basin_group ~ .")
}

indicator_title <- plot_data |>
  dplyr::arrange(.data$indicator_order, .data$indicator_label) |>
  dplyr::distinct(.data$indicator_label) |>
  dplyr::slice(1) |>
  dplyr::pull(.data$indicator_label)

if (is.na(indicator_title) || indicator_title == "") {
  indicator_title <- INDICATOR_FILTER
}

plot_title <- paste0(
  indicator_title,
  " - line trajectories"
)

subtitle_products <- product_summary$product_label
subtitle_products <- subtitle_products[!is.na(subtitle_products)]
subtitle_products <- unique(subtitle_products)

if (length(subtitle_products) == 0) {
  subtitle_products <- "Relative change"
}

plot_subtitle <- paste0(
  paste(subtitle_products, collapse = " / "),
  ", ",
  START_YEAR,
  "-",
  END_YEAR
)

p <- ggplot(
  plot_data,
  aes(
    x = .data$x_value,
    y = .data$value,
    group = .data$line_id,
    colour = .data$scenario_label
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
    aes(shape = .data$model_abbreviation),
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
    subtitle = plot_subtitle,
    x = "Year",
    y = "Change relative to reference (%)",
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

if (AGGREGATE_TO_ANNUAL) {
  p <- p +
    scale_x_continuous(
      breaks = seq(START_YEAR, END_YEAR, by = 5),
      limits = c(START_YEAR, END_YEAR)
    )
} else {
  p <- p +
    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      limits = as.Date(c(
        paste0(START_YEAR, "-01-01"),
        paste0(END_YEAR, "-12-31")
      ))
    )
}

print(p)

# ------------------------------------------------------------
# Output
# ------------------------------------------------------------

OUTPUT_FILE <- make_actnow_plot_filename(
  plot_type = PLOT_TYPE,
  data = plot_data,
  output_dir = OUTPUT_DIR
)

DATA_FILE <- sub("\\.png$", ".csv", OUTPUT_FILE)

# Keep filenames useful but avoid Windows path absurdity for broader filters.
if (nchar(basename(OUTPUT_FILE)) > 140) {
  indicator_slug <- make_actnow_slug(unique(plot_data$indicator))

  OUTPUT_FILE <- file.path(
    OUTPUT_DIR,
    paste0(PLOT_TYPE, "__", indicator_slug, ".png")
  )

  DATA_FILE <- file.path(
    OUTPUT_DIR,
    paste0(PLOT_TYPE, "__", indicator_slug, ".csv")
  )
}

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
