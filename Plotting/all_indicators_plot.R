# ============================================================
# ActNow all-indicator summary by model and scenario
#
# One faceted plot:
#   - one panel per indicator
#   - rows = ActNow models
#   - filled circles = scenarios
#   - conceptual product selected via metadata response_type
#   - supports historical-reference and control-reference products
#     so rCaN can appear alongside the other models
#   - regular filters can limit models, scenarios, indicators, cadence
#   - missing model output folders are skipped quietly
#   - model/scenario/indicator order comes from YAML sort-order fields
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

DATA_ROOT <- "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized"
METADATA_FILE <- "actnow_metadata.yaml"
UTILS_FILE <- "actnow_data_utils_v0_2.r"

OUTPUT_DIR <- "D:/Sources/ActNow"

source(UTILS_FILE)
metadata <- read_actnow_metadata(METADATA_FILE)

# ------------------------------------------------------------
# Plot settings
# ------------------------------------------------------------

# Conceptual product to plot.
#
# The current ActNow utilities support products with different reference types,
# for example:
#   relative_historical_intervention
#   relative_control_intervention
#
# These are the same conceptual output for this plot: climate + management
# change relative to the model-appropriate reference. rCaN uses control
# simulations, so an exact filter for relative_historical_intervention would
# exclude it.
PRODUCT_RESPONSE_TYPE <- "intervention"
REFERENCE_TYPES <- c("historical", "control")

# Fallback product IDs used when the metadata does not yet carry
# response_type/reference_type fields.
FALLBACK_PRODUCT_IDS <- c(
  "relative_historical_intervention",
  "relative_control_intervention"
)

# Optional hard override. Leave NULL for metadata-driven product selection.
PRODUCT_IDS_OVERRIDE <- NULL

# Regular optional regex filters. Use NULL to include all available data.
CASE_STUDY_FILTER <- NULL
MODEL_FILTER <- NULL
SCENARIO_FILTER <- NULL
INDICATOR_FILTER <- NULL
CADENCE_FILTER <- NULL

# Set TRUE for a stricter deliverable plot that only includes indicators
# explicitly known in actnow_metadata.yaml.
DROP_UNKNOWN_INDICATORS <- FALSE

# Facet layout.
FACET_NCOL <- 4

# Curated file names are better for this synthesis plot.
OUTPUT_STEM <- paste0(
  "actnow_all_indicators_by_model_and_scenario__",
  PRODUCT_RESPONSE_TYPE
)

OUTPUT_FILE <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, ".png"))
DATA_FILE <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, ".csv"))

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Filled-circle colours for the standard scenarios.
# Any extra scenarios fall back to ggplot/scales hue colours.
SCENARIO_COLOURS_BY_ID <- c(
  gs = "#F8766D",
  rr = "#7CAE00",
  `rr-cw` = "#619CFF",
  `rr-hw` = "#00BA38",
  `in` = "#00BFC4",
  wm = "#C77CFF"
)

# Axis/legend text.
X_AXIS_LABEL <- "Final change relative to model-specific reference (%)"

# ------------------------------------------------------------
# Small local helpers
# ------------------------------------------------------------

column_or_default <- function(df, column_name, default_value) {
  if (column_name %in% names(df)) {
    return(df[[column_name]])
  }

  rep(default_value, nrow(df))
}

label_or_id <- function(label, id) {
  out <- dplyr::coalesce(as.character(label), as.character(id))
  out[out == ""] <- as.character(id)[out == ""]
  out
}

get_column_or_na <- function(df, column_name) {
  if (column_name %in% names(df)) {
    return(df[[column_name]])
  }

  rep(NA_character_, nrow(df))
}

matches_optional_regex_safe <- function(x, pattern) {
  out <- matches_optional_regex(as.character(x), pattern)
  out[is.na(out)] <- FALSE
  out
}

filter_model_catalogue <- function(models, case_study_filter = NULL, model_filter = NULL) {
  if (nrow(models) == 0) {
    return(models)
  }

  keep_case_study <-
    matches_optional_regex_safe(get_column_or_na(models, "case_study_id"), case_study_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_id"), case_study_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_abbreviation"), case_study_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_label"), case_study_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_root"), case_study_filter)

  keep_model <-
    matches_optional_regex_safe(get_column_or_na(models, "model_id"), model_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_abbreviation"), model_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_label"), model_filter) |
    matches_optional_regex_safe(get_column_or_na(models, "model_root"), model_filter)

  models[keep_case_study & keep_model, , drop = FALSE]
}

make_display_lookup <- function(df, id_column, label_column, order_column, abbreviation_column = NULL) {
  id_values <- as.character(df[[id_column]])
  label_values <- label_or_id(df[[label_column]], id_values)

  if (!is.null(abbreviation_column) && abbreviation_column %in% names(df)) {
    abbreviation_values <- label_or_id(df[[abbreviation_column]], id_values)
  } else {
    abbreviation_values <- id_values
  }

  lookup <- tibble::tibble(
    id = id_values,
    display = label_values,
    order = as.integer(df[[order_column]]),
    abbreviation = abbreviation_values
  ) |>
    dplyr::distinct(id, display, order, abbreviation) |>
    dplyr::arrange(.data$order, .data$display) |>
    dplyr::add_count(display, name = "n_with_same_display") |>
    dplyr::mutate(
      display = dplyr::if_else(
        .data$n_with_same_display > 1,
        paste0(.data$display, " [", .data$abbreviation, "]"),
        .data$display
      )
    ) |>
    dplyr::select(id, display, order)

  lookup
}

make_id_regex <- function(ids) {
  ids <- ids[!is.na(ids)]
  ids <- unique(as.character(ids))

  if (length(ids) == 0) {
    stop("Cannot build product regex from an empty ID vector.")
  }

  paste0("^(", paste(ids, collapse = "|"), ")$")
}

select_conceptual_product_ids <- function(metadata,
                                          response_type,
                                          reference_types,
                                          fallback_product_ids,
                                          override_product_ids = NULL) {
  if (!is.null(override_product_ids)) {
    return(unique(as.character(override_product_ids)))
  }

  products <- metadata$products

  has_response_type <- "response_type" %in% names(products)
  has_reference_type <- "reference_type" %in% names(products)
  has_deprecated <- "deprecated" %in% names(products)

  if (has_response_type && any(!is.na(products$response_type))) {
    deprecated <- if (has_deprecated) {
      products$deprecated
    } else {
      rep(FALSE, nrow(products))
    }

    deprecated <- tidyr::replace_na(as.logical(deprecated), FALSE)

    reference_type <- if (has_reference_type) {
      products$reference_type
    } else {
      rep(NA_character_, nrow(products))
    }

    selected <- products |>
      dplyr::mutate(
        deprecated_flag = deprecated,
        reference_type_for_filter = reference_type
      ) |>
      dplyr::filter(!.data$deprecated_flag) |>
      dplyr::filter(.data$response_type == response_type) |>
      dplyr::filter(
        is.na(.data$reference_type_for_filter) |
          .data$reference_type_for_filter %in% reference_types
      ) |>
      dplyr::arrange(.data$product_order, .data$product) |>
      dplyr::pull(.data$product)

    if (length(selected) > 0) {
      return(unique(as.character(selected)))
    }
  }

  # Fallback for older metadata. Keep the full fallback list even when some
  # products are not listed in metadata, because discovery can still find
  # product folders on disk and mark them as unknown.
  unique(as.character(fallback_product_ids))
}

make_product_subtitle <- function(metadata, product_ids, response_type) {
  products <- metadata$products |>
    dplyr::filter(.data$product %in% product_ids)

  if (nrow(products) == 1) {
    label <- products$product_label[[1]]
    if (!is.na(label) && label != "") {
      return(label)
    }
  }

  if ("response_type" %in% names(products) && any(products$response_type == response_type, na.rm = TRUE)) {
    return("Climate + management change (%) from model-specific reference")
  }

  "Selected ActNow product"
}

# ------------------------------------------------------------
# Select conceptual product IDs
# ------------------------------------------------------------

PRODUCT_IDS <- select_conceptual_product_ids(
  metadata = metadata,
  response_type = PRODUCT_RESPONSE_TYPE,
  reference_types = REFERENCE_TYPES,
  fallback_product_ids = FALLBACK_PRODUCT_IDS,
  override_product_ids = PRODUCT_IDS_OVERRIDE
)

PRODUCT_FILTER <- make_id_regex(PRODUCT_IDS)

product_subtitle <- make_product_subtitle(
  metadata = metadata,
  product_ids = PRODUCT_IDS,
  response_type = PRODUCT_RESPONSE_TYPE
)

message(
  "Plotting conceptual product using product folders:\n",
  paste0("  - ", PRODUCT_IDS, collapse = "\n")
)

# ------------------------------------------------------------
# Prepare metadata for plotting/discovery
# ------------------------------------------------------------

metadata_for_plot <- metadata

# The authoritative utilities already promote YAML sort-order into model_order,
# scenario_order, product_order and indicator_order. The generic sort_order
# columns are no longer needed for plotting and can otherwise create duplicate
# sort_order.x/sort_order.y columns during joins.
metadata_for_plot$models <- metadata_for_plot$models |>
  dplyr::select(-dplyr::any_of("sort_order"))

metadata_for_plot$products <- metadata_for_plot$products |>
  dplyr::select(-dplyr::any_of("sort_order"))

metadata_for_plot$scenarios <- metadata_for_plot$scenarios |>
  dplyr::select(-dplyr::any_of("sort_order"))

metadata_for_plot$indicators <- metadata_for_plot$indicators |>
  dplyr::select(-dplyr::any_of("sort_order"))

# ------------------------------------------------------------
# Filter model catalogue and skip missing model output folders
# ------------------------------------------------------------

candidate_models <- metadata_for_plot$models |>
  filter_model_catalogue(
    case_study_filter = CASE_STUDY_FILTER,
    model_filter = MODEL_FILTER
  ) |>
  dplyr::mutate(
    model_output_path = file.path(DATA_ROOT, .data$model_root),
    model_output_exists = dir.exists(.data$model_output_path)
  )

if (nrow(candidate_models) == 0) {
  stop("No models matched CASE_STUDY_FILTER and MODEL_FILTER.")
}

missing_model_outputs <- candidate_models |>
  dplyr::filter(!.data$model_output_exists)

if (nrow(missing_model_outputs) > 0) {
  message(
    "Skipping models with no output folder:\n",
    paste0(
      "  - ",
      missing_model_outputs$model_label,
      " [",
      missing_model_outputs$model_id,
      "]",
      collapse = "\n"
    )
  )
}

metadata_for_plot$models <- candidate_models |>
  dplyr::filter(.data$model_output_exists) |>
  dplyr::select(-model_output_path, -model_output_exists)

if (nrow(metadata_for_plot$models) == 0) {
  stop("No models with output folders remain after filtering.")
}

# ------------------------------------------------------------
# Discover and load data
# ------------------------------------------------------------

files <- discover_actnow_files(
  data_root = DATA_ROOT,
  metadata = metadata_for_plot,
  case_study_filter = NULL,              # already applied above
  scenario_filter = SCENARIO_FILTER,
  indicator_filter = INDICATOR_FILTER,
  product_filter = PRODUCT_FILTER,
  cadence_filter = CADENCE_FILTER,
  prefer_cadence = TRUE
) |>
  dplyr::filter(.data$product %in% PRODUCT_IDS) |>
  dplyr::filter(file.exists(.data$source_file))

if (nrow(files) == 0) {
  stop("No files found after applying filters.")
}

message(
  "Discovered ", nrow(files), " files across ",
  dplyr::n_distinct(files$model_id), " models, ",
  dplyr::n_distinct(files$product), " product folders, ",
  dplyr::n_distinct(files$scenario), " scenarios and ",
  dplyr::n_distinct(files$indicator), " indicators."
)

message("Product folders discovered:")
print(files |> dplyr::count(.data$product, name = "n_files") |> dplyr::arrange(.data$product))

actnow_data <- load_actnow_data(files)

if (nrow(actnow_data) == 0) {
  stop("Files were discovered, but no data rows were loaded.")
}

# ------------------------------------------------------------
# Select final values and prepare plotting data
# ------------------------------------------------------------

plot_df <- select_final_actnow_values(actnow_data) |>
  dplyr::mutate(
    model_label = label_or_id(.data$model_label, .data$model_id),
    scenario_label = label_or_id(.data$scenario_label, .data$scenario),
    indicator_label = label_or_id(.data$indicator_label, .data$indicator),
    model_order = dplyr::coalesce(as.integer(.data$model_order), 9999L),
    scenario_order = dplyr::coalesce(as.integer(.data$scenario_order), 9999L),
    indicator_order = dplyr::coalesce(as.integer(.data$indicator_order), 9999L),
    product_order = dplyr::coalesce(as.integer(.data$product_order), 9999L),
    product_choice_order = match(.data$product, PRODUCT_IDS),
    product_choice_order = dplyr::if_else(
      is.na(.data$product_choice_order),
      9999L,
      as.integer(.data$product_choice_order)
    )
  ) |>
  dplyr::filter(!is.na(.data$value))

# If a model somehow has both historical-reference and control-reference
# versions for the same scenario/indicator, keep one conceptual-product row.
plot_df <- plot_df |>
  dplyr::group_by(.data$model_id, .data$scenario, .data$indicator) |>
  dplyr::arrange(.data$product_choice_order, .data$product_order, .by_group = TRUE) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

if (DROP_UNKNOWN_INDICATORS && "is_known_indicator" %in% names(plot_df)) {
  plot_df <- plot_df |>
    dplyr::filter(.data$is_known_indicator)
}

if (nrow(plot_df) == 0) {
  stop("plot_df is empty after selecting final values and applying plot filters.")
}

if ("is_known_indicator" %in% names(plot_df)) {
  unknown_indicators <- plot_df |>
    dplyr::filter(!.data$is_known_indicator) |>
    dplyr::distinct(indicator) |>
    dplyr::arrange(indicator)

  if (nrow(unknown_indicators) > 0) {
    message(
      "Indicators found on disk but not fully known in metadata:\n",
      paste0("  - ", unknown_indicators$indicator, collapse = "\n")
    )
  }
}

# Build display labels and factor levels from actual plotted data only.
model_lookup <- make_display_lookup(
  df = plot_df,
  id_column = "model_id",
  label_column = "model_label",
  order_column = "model_order",
  abbreviation_column = "model_abbreviation"
) |>
  dplyr::rename(
    model_id = id,
    model_display = display,
    model_display_order = order
  )

scenario_lookup <- make_display_lookup(
  df = plot_df,
  id_column = "scenario",
  label_column = "scenario_label",
  order_column = "scenario_order",
  abbreviation_column = "scenario_abbreviation"
) |>
  dplyr::rename(
    scenario = id,
    scenario_display = display,
    scenario_display_order = order
  )

indicator_lookup <- make_display_lookup(
  df = plot_df,
  id_column = "indicator",
  label_column = "indicator_label",
  order_column = "indicator_order",
  abbreviation_column = "indicator_abbreviation"
) |>
  dplyr::rename(
    indicator = id,
    indicator_display = display,
    indicator_display_order = order
  )

model_levels <- model_lookup |>
  dplyr::arrange(.data$model_display_order, .data$model_display) |>
  dplyr::pull(model_display)

scenario_levels <- scenario_lookup |>
  dplyr::arrange(.data$scenario_display_order, .data$scenario_display) |>
  dplyr::pull(scenario_display)

indicator_levels <- indicator_lookup |>
  dplyr::arrange(.data$indicator_display_order, .data$indicator_display) |>
  dplyr::pull(indicator_display)

plot_df <- plot_df |>
  dplyr::left_join(model_lookup |> dplyr::select(model_id, model_display), by = "model_id") |>
  dplyr::left_join(scenario_lookup |> dplyr::select(scenario, scenario_display), by = "scenario") |>
  dplyr::left_join(indicator_lookup |> dplyr::select(indicator, indicator_display), by = "indicator") |>
  dplyr::mutate(
    # Reverse models so the first YAML sort-order appears at the top.
    model_display = factor(.data$model_display, levels = rev(model_levels)),
    scenario_display = factor(.data$scenario_display, levels = scenario_levels),
    indicator_display = factor(.data$indicator_display, levels = indicator_levels)
  )

# Scenario colour vector named by display labels.
scenario_colour_tbl <- scenario_lookup |>
  dplyr::arrange(.data$scenario_display_order, .data$scenario_display) |>
  dplyr::mutate(
    colour = unname(SCENARIO_COLOURS_BY_ID[as.character(.data$scenario)])
  )

missing_colour <- is.na(scenario_colour_tbl$colour)
if (any(missing_colour)) {
  scenario_colour_tbl$colour[missing_colour] <- scales::hue_pal()(
    sum(missing_colour)
  )
}

scenario_colours <- stats::setNames(
  scenario_colour_tbl$colour,
  scenario_colour_tbl$scenario_display
)

# Export exactly the dataframe used for plotting.
readr::write_csv(plot_df, DATA_FILE)

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

p <- ggplot(
  plot_df,
  aes(
    x = .data$value,
    y = .data$model_display
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.35,
    colour = "grey35"
  ) +
  geom_point(
    aes(colour = .data$scenario_display),
    shape = 16,
    size = 2.3,
    alpha = 0.95
  ) +
  facet_wrap(
    ~ indicator_display,
    scales = "free_x",
    ncol = FACET_NCOL,
    labeller = label_wrap_gen(width = 28)
  ) +
  scale_colour_manual(
    values = scenario_colours,
    breaks = scenario_levels,
    drop = FALSE
  ) +
  scale_y_discrete(
    labels = function(x) stringr::str_wrap(x, width = 28)
  ) +
  labs(
    title = "ActNow indicators by model and scenario",
    subtitle = product_subtitle,
    x = X_AXIS_LABEL,
    y = "Model",
    colour = "Scenario"
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        shape = 16,
        size = 3,
        alpha = 1
      )
    )
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    strip.background = element_rect(fill = "white", colour = "grey60"),
    strip.text = element_text(size = 8.5),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey75"),
    panel.grid.major.x = element_line(linewidth = 0.20, colour = "grey88"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7.5),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

# Dynamic output size for changing numbers of indicators/models.
n_models <- dplyr::n_distinct(plot_df$model_id)
n_indicators <- dplyr::n_distinct(plot_df$indicator)
n_facet_rows <- ceiling(n_indicators / FACET_NCOL)
panel_row_height <- max(1.35, 0.22 * n_models + 0.65)

plot_width <- max(12, 3.7 * FACET_NCOL)
plot_height <- max(7, 1.8 + n_facet_rows * panel_row_height)

ggsave(
  filename = OUTPUT_FILE,
  plot = p,
  width = plot_width,
  height = plot_height,
  dpi = 300,
  limitsize = FALSE
)

message("Wrote figure: ", OUTPUT_FILE)
message("Wrote plotted data: ", DATA_FILE)

p
