# ============================================================
# ActNow paired lollipop plot - reference-aware version
# ============================================================
# Purpose:
#   Plot paired climate-only and climate+management responses from the
#   standardised ActNow output folders.
#
# Data logic:
#   The adapter layer now writes explicit relative products:
#
#     relative_historical_climate
#     relative_historical_intervention
#     relative_control_climate
#     relative_control_intervention
#
#   The first pair is used for models that compare against a fixed
#   historical reference. The second pair is used for models that compare
#   against a control trajectory, such as rCaN.
#
#   This plotting script does not need to know how the reference value was
#   calculated. It only requires that the relative products are expressed as
#   percentage change relative to the applicable reference.
#
# Expected folder contract:
#   <model_root>/<product>/<scenario>/<cadence>/<indicator>.csv
# ============================================================

library(tidyverse)
library(forcats)

# ------------------------------------------------------------
# User settings
# ------------------------------------------------------------

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"
PLOT_TYPE <- "paired_lollipop"

# Regex filters. Leave NULL to include all available data.
INDICATOR_FILTER <- "consumer_catch"
CASE_STUDY_FILTER <- NULL
SCENARIO_FILTER <- NULL

# Paired lollipops require one climate product and one intervention product
# relative to the same reference type. This may be historical or control.
PRODUCT_FILTER <- paste(
  c(
    "relative_historical_climate",
    "relative_historical_intervention",
    "relative_control_climate",
    "relative_control_intervention"
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
# Small local helpers
# ------------------------------------------------------------

add_reference_product_fields <- function(data) {
  # Prefer metadata-driven fields when available. If older metadata is used,
  # derive the fields from product names so the plot fails less mysteriously.
  if (!"reference_type" %in% names(data)) {
    data$reference_type <- NA_character_
  }

  if (!"response_type" %in% names(data)) {
    data$response_type <- NA_character_
  }

  data |>
    mutate(
      reference_type = case_when(
        !is.na(.data$reference_type) ~ .data$reference_type,
        str_detect(.data$product, "^relative_historical_") ~ "historical",
        str_detect(.data$product, "^relative_control_") ~ "control",
        TRUE ~ NA_character_
      ),
      response_type = case_when(
        !is.na(.data$response_type) ~ .data$response_type,
        str_detect(.data$product, "_climate$") ~ "climate",
        str_detect(.data$product, "_intervention$") ~ "intervention",
        TRUE ~ NA_character_
      ),
      reference_label = case_when(
        .data$reference_type == "historical" ~ "Historical reference",
        .data$reference_type == "control" ~ "Control reference",
        TRUE ~ "Reference"
      )
    )
}

make_safe_plot_filename <- function(plot_type, data) {
  if (exists("make_actnow_plot_filename")) {
    return(make_actnow_plot_filename(plot_type = plot_type, data = data))
  }

  indicator_part <- data |>
    distinct(.data$indicator) |>
    pull(.data$indicator) |>
    paste(collapse = "-")

  if (indicator_part == "") {
    indicator_part <- "indicator"
  }

  paste0(plot_type, "__", indicator_part, ".png")
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

files <- add_reference_product_fields(files)

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

actnow_data <- add_reference_product_fields(actnow_data)

if (exists("print_actnow_data_diagnostics")) {
  print_actnow_data_diagnostics(actnow_data)
}

final_values <- select_final_actnow_values(actnow_data)
final_values <- add_reference_product_fields(final_values)

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
  "reference_type",
  "reference_label",
  "response_type",
  "value"
)

missing_columns <- setdiff(required_columns, names(final_values))
if (length(missing_columns) > 0) {
  stop(
    "Loaded data is missing required columns for plotting: ",
    paste(missing_columns, collapse = ", ")
  )
}

relative_values <- final_values |>
  filter(
    .data$indicator == INDICATOR_FILTER,
    .data$reference_type %in% c("historical", "control"),
    .data$response_type %in% c("climate", "intervention")
  ) |>
  select(all_of(required_columns)) |>
  mutate(
    # Climate-only products may be stored under a parent scenario, e.g. rr,
    # while intervention variants may be stored under rr-cw and rr-hw.
    # Join via scenario_family so variants inherit the correct climate-only
    # reference point without duplicating data in the adapter.
    scenario_pair_key = coalesce(.data$scenario_family, .data$scenario)
  )

climate_rows <- relative_values |>
  filter(.data$response_type == "climate") |>
  select(
    model_id,
    reference_type,
    indicator,
    scenario_pair_key,
    climate_product = product,
    climate_scenario = scenario,
    climate_only = value
  )

intervention_rows <- relative_values |>
  filter(.data$response_type == "intervention") |>
  select(
    model_id,
    model_abbreviation,
    model_label,
    case_study_id,
    scenario,
    scenario_label,
    scenario_family,
    scenario_variant,
    scenario_order,
    indicator,
    indicator_label,
    reference_type,
    reference_label,
    scenario_pair_key,
    intervention_product = product,
    climate_management = value
  )

paired_source <- intervention_rows |>
  left_join(
    climate_rows,
    by = c(
      "model_id",
      "reference_type",
      "indicator",
      "scenario_pair_key"
    )
  ) |>
  filter(
    !is.na(.data$climate_only),
    !is.na(.data$climate_management)
  ) |>
  mutate(
    # Temporary grouping until basin/case metadata is represented explicitly
    # in actnow_metadata.yaml.
    basin_group = case_when(
      .data$case_study_id %in% c("cs01", "cs02") ~ "Arctic",
      .data$case_study_id %in% c("cs05", "cs06") ~ "North Sea",
      .data$case_study_id %in% c("cs07") ~ "Black Sea",
      .data$case_study_id %in% c("cs11") ~ "Mediterranean",
      TRUE ~ .data$case_study_id
    ),

    display_label = .data$scenario_label,
    pair_id = paste(.data$model_abbreviation, .data$display_label, sep = " - ")
  )

if (nrow(paired_source) == 0) {
  stop(
    "No complete paired rows found. Need both climate and intervention ",
    "relative products for the selected indicator."
  )
}

# Make sure each model/scenario/reference/indicator gives one paired row.
duplicate_pairs <- paired_source |>
  count(
    .data$model_id,
    .data$scenario,
    .data$reference_type,
    .data$indicator,
    name = "n"
  ) |>
  filter(.data$n > 1)

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
    factor(.data$basin_group, levels = basin_levels),
    .data$model_abbreviation,
    .data$reference_type,
    .data$scenario_order,
    .data$scenario
  ) |>
  mutate(
    basin_group = factor(.data$basin_group, levels = basin_levels),
    pair_id = factor(.data$pair_id, levels = rev(unique(.data$pair_id))),
    reference_type = factor(
      .data$reference_type,
      levels = c("historical", "control")
    )
  )

point_df <- paired_df |>
  select(
    indicator,
    indicator_label,
    reference_type,
    reference_label,
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
    basin_group = factor(.data$basin_group, levels = basin_levels),
    pair_id = factor(.data$pair_id, levels = levels(paired_df$pair_id)),
    run_type = factor(
      .data$run_type,
      levels = c("climate_only", "climate_management")
    )
  )

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

cat("\nPaired rows used for plot:\n")
print(
  paired_df |>
    count(
      .data$basin_group,
      .data$model_abbreviation,
      .data$reference_type,
      .data$scenario,
      name = "n"
    ) |>
    arrange(
      .data$basin_group,
      .data$model_abbreviation,
      .data$reference_type,
      .data$scenario
    ),
  n = Inf
)
cat("\n")

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

indicator_title <- unique(na.omit(paired_df$indicator_label))[1]
if (is.na(indicator_title)) {
  indicator_title <- unique(na.omit(paired_df$indicator))[1]
}

plot_title <- paste0(
  indicator_title,
  " - paired climate and management effects"
)

p <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey35"
  ) +

  # Thin grey line: applicable reference to climate-only.
  geom_segment(
    data = paired_df,
    aes(
      x = 0,
      xend = .data$climate_only,
      y = .data$pair_id,
      yend = .data$pair_id
    ),
    colour = "grey75",
    linewidth = 0.55
  ) +

  # Thick coloured line: climate-only to climate+management.
  geom_segment(
    data = paired_df,
    aes(
      x = .data$climate_only,
      xend = .data$climate_management,
      y = .data$pair_id,
      yend = .data$pair_id,
      colour = .data$display_label
    ),
    linewidth = 1.2
  ) +

  geom_point(
    data = point_df,
    aes(
      x = .data$value,
      y = .data$pair_id,
      colour = .data$display_label,
      shape = .data$run_type
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
    x = "Percentage change relative to reference (%)",
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

OUTPUT_FILE <- make_safe_plot_filename(
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
