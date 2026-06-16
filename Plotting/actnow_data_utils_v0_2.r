# ============================================================
# ActNow shared data utilities
# v0.2: file discovery + metadata enrichment + diagnostics
# ============================================================
# Purpose:
#   Provide a plot-agnostic file index for standardised ActNow outputs.
#   This version deliberately stops at file discovery. Loading CSV contents
#   into long-form plotting dataframes will be added after discovery is stable.
#
# Expected folder contract:
#   <data_root>/<model_root>/<product>/<scenario>/<cadence>/<indicator>.csv
#
# Example:
#   source("actnow_data_utils_v0_2.R")
#
#   metadata <- read_actnow_metadata("actnow_metadata.yaml")
#
#   files <- discover_actnow_files(
#     data_root = "P:/Projects/ActNow/WP3/T3-3/Collaborative modelling/Model output standardized",
#     metadata = metadata,
#     product_filter = "relative_historical",
#     indicator_filter = "total_biomass|mean_trophic_level"
#   )
#
#   summarise_actnow_file_index(files)
#   diagnose_actnow_file_index(files)
#
# Joining rules:
# ==============
# - Never join on labels.
# - Never join on abbreviations.
# - Never join on case study names.
#
# Join on:
# - model_root
# or
# - model_id
#
# Display:
# - model_label
# - basin_label
# - scenario_label
# ============================================================

library(tidyverse)
library(yaml)

# ------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}

strip_for_analysis_suffix <- function(path) {
  # Metadata may point to "<model>/for_analysis" while the standardised
  # output root may be "<model>" directly. Discovery uses the stripped root.
  str_remove(path, "[/\\\\]for_analysis$")
}

matches_optional_regex <- function(x, pattern) {
  if (is.null(pattern) || length(pattern) == 0 || is.na(pattern) || pattern == "") {
    return(rep(TRUE, length(x)))
  }

  str_detect(x, regex(pattern, ignore_case = TRUE))
}

metadata_list_to_tbl <- function(items, table_name) {
  if (is.null(items) || length(items) == 0) {
    return(tibble())
  }

  rows <- lapply(items, function(item) {
    # YAML originally used sort-order. Normalise to sort_order in R.
    if (!is.null(item[["sort-order"]]) && is.null(item[["sort_order"]])) {
      item[["sort_order"]] <- item[["sort-order"]]
    }
    item[["sort-order"]] <- NULL

    as_tibble(item)
  })

  bind_rows(rows) |>
    mutate(metadata_table = table_name, .before = 1)
}

add_missing_column <- function(df, column_name, default_value = NA_character_) {
  if (!column_name %in% names(df)) {
    df[[column_name]] <- default_value
  }

  df
}

# ------------------------------------------------------------
# Metadata reader
# ------------------------------------------------------------

read_actnow_metadata <- function(metadata_file) {
  raw <- yaml::read_yaml(metadata_file)

  models <- metadata_list_to_tbl(raw$models, "models") |>
    rename(
      model_id = id,
      model_abbreviation = abbreviation,
      model_label = label,
      model_root_raw = root
    ) |>
    mutate(
      model_root = strip_for_analysis_suffix(model_root_raw),
      case_study_id = str_extract(model_id, "^cs[0-9]+"),
      model_order = row_number()
    ) |>
    select(-metadata_table)

  products <- metadata_list_to_tbl(raw$products, "products") |>
    rename(
      product = id,
      product_abbreviation = abbreviation,
      product_label = label
    ) |>
    mutate(product_order = row_number()) |>
    select(-metadata_table)

  scenarios <- metadata_list_to_tbl(raw$scenarios, "scenarios") |>
    add_missing_column("parent", NA_character_) |>
    rename(
      scenario = id,
      scenario_abbreviation = abbreviation,
      scenario_label = label,
      scenario_parent = parent
    ) |>
    mutate(
      scenario_family = if_else(is.na(scenario_parent), scenario, scenario_parent),
      scenario_variant = if_else(
        is.na(scenario_parent),
        NA_character_,
        str_remove(scenario, paste0("^", scenario_parent, "-"))
      ),
      scenario_order = row_number()
    ) |>
    select(-metadata_table)

  indicators <- metadata_list_to_tbl(raw$indicators, "indicators") |>
    add_missing_column("sort_order", NA_integer_) |>
    rename(
      indicator = id,
      indicator_abbreviation = abbreviation,
      indicator_label = label,
      indicator_tag = tag
    ) |>
    mutate(
      indicator_order = if_else(is.na(sort_order), row_number(), as.integer(sort_order))
    ) |>
    select(-metadata_table)

  list(
    version = raw$version,
    data_layout = raw$data_layout,
    cadence_preference = raw$cadence_preference %||% c("monthly", "annual"),
    models = models,
    products = products,
    scenarios = scenarios,
    indicators = indicators
  )
}

# ------------------------------------------------------------
# File discovery
# ------------------------------------------------------------

discover_actnow_files <- function(data_root,
                                  metadata,
                                  case_study_filter = NULL,
                                  scenario_filter = NULL,
                                  indicator_filter = NULL,
                                  product_filter = NULL,
                                  cadence_filter = NULL,
                                  prefer_cadence = TRUE) {
  models <- metadata$models |>
    filter(
      matches_optional_regex(case_study_id, case_study_filter) |
        matches_optional_regex(model_id, case_study_filter) |
        matches_optional_regex(model_abbreviation, case_study_filter) |
        matches_optional_regex(model_label, case_study_filter) |
        matches_optional_regex(model_root, case_study_filter)
    )

  if (nrow(models) == 0) {
    warning("No models matched case_study_filter.")
    return(tibble())
  }

  model_file_indices <- lapply(seq_len(nrow(models)), function(i) {
    model <- models[i, ]
    model_path <- file.path(data_root, model$model_root)

    if (!dir.exists(model_path)) {
      warning(paste("Model folder does not exist:", model_path))
      return(tibble())
    }

    csv_files <- list.files(
      model_path,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(csv_files) == 0) {
      return(tibble())
    }

    relative_paths <- str_remove(csv_files, paste0("^", fixed(model_path), "[/\\\\]?"))
    parts <- str_split(relative_paths, "[/\\\\]", simplify = TRUE)

    # Expected: product/scenario/cadence/indicator.csv.
    # Anything else is ignored, but retained in diagnostics by n_ignored_paths.
    valid <- ncol(parts) >= 4

    tibble(
      source_file = csv_files[valid],
      relative_path = relative_paths[valid],
      model_id = model$model_id,
      product = parts[valid, 1],
      scenario = parts[valid, 2],
      cadence = parts[valid, 3],
      indicator = str_remove(basename(parts[valid, 4]), "\\.csv$")
    )
  })

  files <- bind_rows(model_file_indices)

  if (nrow(files) == 0) {
    return(files)
  }

  files <- files |>
    filter(matches_optional_regex(product, product_filter)) |>
    filter(matches_optional_regex(scenario, scenario_filter)) |>
    filter(matches_optional_regex(indicator, indicator_filter)) |>
    filter(matches_optional_regex(cadence, cadence_filter)) |>
    left_join(metadata$models, by = "model_id") |>
    left_join(metadata$products, by = "product") |>
    left_join(metadata$scenarios, by = "scenario") |>
    left_join(metadata$indicators, by = "indicator") |>
    mutate(
      scenario_family = coalesce(scenario_family, scenario),
      indicator_order = coalesce(indicator_order, 9999L),
      scenario_order = coalesce(scenario_order, 9999L),
      product_order = coalesce(product_order, 9999L),
      cadence_order = match(cadence, metadata$cadence_preference),
      cadence_order = if_else(is.na(cadence_order), 9999L, as.integer(cadence_order)),
      is_known_product = !is.na(product_label),
      is_known_scenario = !is.na(scenario_label),
      is_known_indicator = !is.na(indicator_label)
    )

  if (prefer_cadence) {
    files <- files |>
      group_by(model_id, product, scenario, indicator) |>
      arrange(cadence_order, .by_group = TRUE) |>
      slice(1) |>
      ungroup()
  }

  files |>
    arrange(
      model_order,
      product_order,
      scenario_order,
      cadence_order,
      indicator_order,
      indicator
    )
}

# ------------------------------------------------------------
# Convenience diagnostics
# ------------------------------------------------------------

summarise_actnow_file_index <- function(file_index) {
  if (nrow(file_index) == 0) {
    return(tibble())
  }

  file_index |>
    count(
      case_study_id,
      model_id,
      model_abbreviation,
      product,
      scenario,
      cadence,
      name = "n_indicators"
    ) |>
    arrange(case_study_id, model_id, product, scenario, cadence)
}

diagnose_actnow_file_index <- function(file_index) {
  if (nrow(file_index) == 0) {
    return(list(
      duplicate_source_files = tibble(),
      unknown_products = tibble(),
      unknown_scenarios = tibble(),
      unknown_indicators = tibble(),
      model_roots = tibble()
    ))
  }

  duplicate_source_files <- file_index |>
    count(source_file, name = "n") |>
    filter(n > 1) |>
    arrange(desc(n), source_file)

  unknown_products <- file_index |>
    filter(!is_known_product) |>
    distinct(product) |>
    arrange(product)

  unknown_scenarios <- file_index |>
    filter(!is_known_scenario) |>
    distinct(scenario) |>
    arrange(scenario)

  unknown_indicators <- file_index |>
    filter(!is_known_indicator) |>
    distinct(indicator) |>
    arrange(indicator)

  model_roots <- file_index |>
    distinct(
      case_study_id,
      model_id,
      model_abbreviation,
      model_label,
      model_root
    ) |>
    arrange(case_study_id, model_id)

  list(
    duplicate_source_files = duplicate_source_files,
    unknown_products = unknown_products,
    unknown_scenarios = unknown_scenarios,
    unknown_indicators = unknown_indicators,
    model_roots = model_roots
  )
}

print_actnow_file_diagnostics <- function(file_index) {
  diagnostics <- diagnose_actnow_file_index(file_index)

  cat("Duplicate source files:", nrow(diagnostics$duplicate_source_files), "\n")
  cat("Unknown products:", nrow(diagnostics$unknown_products), "\n")
  cat("Unknown scenarios:", nrow(diagnostics$unknown_scenarios), "\n")
  cat("Unknown indicators:", nrow(diagnostics$unknown_indicators), "\n")

  invisible(diagnostics)
}

load_actnow_data <- function(files)
{
  required_columns <- c("source_file", "indicator", "cadence", "scenario", "product", "model_id")
  
  missing_columns <- setdiff(required_columns, names(files))
  if (length(missing_columns) > 0) {
    stop(
      "files is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (nrow(files) == 0) {
    return(tibble::tibble())
  }
  
  loaded <- list()
  
  for (i in seq_len(nrow(files))) {
    source_file <- files$source_file[[i]]
    
    if (!file.exists(source_file)) {
      warning("Skipping missing file: ", source_file)
      next
    }
    
    df <- readr::read_csv(
      source_file,
      show_col_types = FALSE
    )
    
    if (!"date" %in% names(df)) {
      warning("Skipping file without 'date' column: ", source_file)
      next
    }
    
    if (!"value" %in% names(df)) {
      warning("Skipping file without 'value' column: ", source_file)
      next
    }
    
    metadata_row <- files[i, , drop = FALSE]
    
    for (column_name in names(metadata_row)) {
      df[[column_name]] <- metadata_row[[column_name]][[1]]
    }
    
    df <- df |>
      dplyr::mutate(
        date = as.Date(.data$date),
        value = as.numeric(.data$value)
      )
    
    loaded[[length(loaded) + 1]] <- df
  }
  
  if (length(loaded) == 0) {
    return(tibble::tibble())
  }
  
  dplyr::bind_rows(loaded)
}


diagnose_actnow_data <- function(data)
{
  required_columns <- c(
    "source_file",
    "model_id",
    "product",
    "scenario",
    "cadence",
    "indicator",
    "date",
    "value"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    return(list(
      missing_columns = missing_columns
    ))
  }
  
  list(
    missing_columns = tibble::tibble(column = character()),
    
    missing_dates = data |>
      dplyr::filter(is.na(.data$date)) |>
      dplyr::count(.data$source_file, name = "n_missing_dates"),
    
    missing_values = data |>
      dplyr::filter(is.na(.data$value)) |>
      dplyr::count(.data$source_file, name = "n_missing_values"),
    
    duplicated_observations = data |>
      dplyr::count(
        .data$model_id,
        .data$product,
        .data$scenario,
        .data$cadence,
        .data$indicator,
        .data$date,
        name = "n"
      ) |>
      dplyr::filter(.data$n > 1),
    
    date_ranges = data |>
      dplyr::group_by(
        .data$model_id,
        .data$product,
        .data$scenario,
        .data$cadence,
        .data$indicator
      ) |>
      dplyr::summarise(
        min_date = min(.data$date, na.rm = TRUE),
        max_date = max(.data$date, na.rm = TRUE),
        n_dates = dplyr::n_distinct(.data$date),
        n_values = dplyr::n(),
        .groups = "drop"
      )
  )
}


print_actnow_data_diagnostics <- function(data)
{
  diagnostics <- diagnose_actnow_data(data)
  
  if ("missing_columns" %in% names(diagnostics)) {
    if (length(diagnostics$missing_columns) > 0) {
      cat("Missing required data columns:\n")
      print(diagnostics$missing_columns)
      return(invisible(diagnostics))
    }
  }
  
  cat("Rows loaded: ", nrow(data), "\n", sep = "")
  cat("Files loaded: ", dplyr::n_distinct(data$source_file), "\n", sep = "")
  cat("Models: ", dplyr::n_distinct(data$model_id), "\n", sep = "")
  cat("Indicators: ", dplyr::n_distinct(data$indicator), "\n", sep = "")
  cat("Scenarios: ", dplyr::n_distinct(data$scenario), "\n", sep = "")
  cat("\n")
  
  cat("Missing dates:\n")
  print(diagnostics$missing_dates)
  cat("\n")
  
  cat("Missing values:\n")
  print(diagnostics$missing_values)
  cat("\n")
  
  cat("Duplicated observations:\n")
  print(diagnostics$duplicated_observations)
  cat("\n")
  
  invisible(diagnostics)
}

select_final_actnow_values <- function(data)
{
  required_columns <- c(
    "model_id",
    "product",
    "scenario",
    "cadence",
    "indicator",
    "date",
    "value"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "data is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  data |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::group_by(
      .data$model_id,
      .data$product,
      .data$scenario,
      .data$cadence,
      .data$indicator
    ) |>
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
    dplyr::ungroup()
}


summarise_actnow_periods <- function(data, periods, value_method = "mean")
{
  required_data_columns <- c(
    "model_id",
    "product",
    "scenario",
    "cadence",
    "indicator",
    "date",
    "value"
  )
  
  required_period_columns <- c("period", "start_date", "end_date")
  
  missing_data_columns <- setdiff(required_data_columns, names(data))
  if (length(missing_data_columns) > 0) {
    stop(
      "data is missing required columns: ",
      paste(missing_data_columns, collapse = ", ")
    )
  }
  
  missing_period_columns <- setdiff(required_period_columns, names(periods))
  if (length(missing_period_columns) > 0) {
    stop(
      "periods is missing required columns: ",
      paste(missing_period_columns, collapse = ", ")
    )
  }
  
  periods <- periods |>
    dplyr::mutate(
      start_date = as.Date(.data$start_date),
      end_date = as.Date(.data$end_date)
    )
  
  grouped_columns <- c(
    setdiff(names(data), c("date", "value", "source_file")),
    "period"
  )
  
  output <- list()
  
  for (i in seq_len(nrow(periods))) {
    period_row <- periods[i, , drop = FALSE]
    
    period_data <- data |>
      dplyr::filter(
        !is.na(.data$date),
        .data$date >= period_row$start_date[[1]],
        .data$date <= period_row$end_date[[1]]
      ) |>
      dplyr::mutate(period = period_row$period[[1]])
    
    if (nrow(period_data) == 0) {
      next
    }
    
    if (value_method == "mean") {
      summary <- period_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouped_columns))) |>
        dplyr::summarise(
          value = mean(.data$value, na.rm = TRUE),
          start_date = min(.data$date, na.rm = TRUE),
          end_date = max(.data$date, na.rm = TRUE),
          n_dates = dplyr::n_distinct(.data$date),
          .groups = "drop"
        )
    } else if (value_method == "median") {
      summary <- period_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouped_columns))) |>
        dplyr::summarise(
          value = median(.data$value, na.rm = TRUE),
          start_date = min(.data$date, na.rm = TRUE),
          end_date = max(.data$date, na.rm = TRUE),
          n_dates = dplyr::n_distinct(.data$date),
          .groups = "drop"
        )
    } else if (value_method == "final") {
      summary <- period_data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouped_columns))) |>
        dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
        dplyr::summarise(
          value = dplyr::first(.data$value),
          start_date = min(.data$date, na.rm = TRUE),
          end_date = max(.data$date, na.rm = TRUE),
          n_dates = dplyr::n_distinct(.data$date),
          .groups = "drop"
        )
    } else {
      stop(
        "Unsupported value_method: ",
        value_method,
        ". Use 'mean', 'median', or 'final'."
      )
    }
    
    output[[length(output) + 1]] <- summary
  }
  
  if (length(output) == 0) {
    return(tibble::tibble())
  }
  
  dplyr::bind_rows(output)
}


diagnose_actnow_period_summary <- function(period_data)
{
  required_columns <- c(
    "model_id",
    "product",
    "scenario",
    "cadence",
    "indicator",
    "period",
    "value",
    "n_dates"
  )
  
  missing_columns <- setdiff(required_columns, names(period_data))
  if (length(missing_columns) > 0) {
    return(list(
      missing_columns = missing_columns
    ))
  }
  
  list(
    missing_values = period_data |>
      dplyr::filter(is.na(.data$value)) |>
      dplyr::count(
        .data$model_id,
        .data$product,
        .data$scenario,
        .data$indicator,
        .data$period,
        name = "n_missing_values"
      ),
    
    empty_or_single_point_periods = period_data |>
      dplyr::filter(.data$n_dates <= 1) |>
      dplyr::select(
        .data$model_id,
        .data$product,
        .data$scenario,
        .data$indicator,
        .data$period,
        .data$n_dates
      ),
    
    duplicated_period_rows = period_data |>
      dplyr::count(
        .data$model_id,
        .data$product,
        .data$scenario,
        .data$cadence,
        .data$indicator,
        .data$period,
        name = "n"
      ) |>
      dplyr::filter(.data$n > 1)
  )
}

make_actnow_slug <- function(values, max_items = 4)
{
  values <- values[!is.na(values)]
  values <- sort(unique(as.character(values)))
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  if (length(values) > max_items) {
    slug <- paste0(
      paste(values[seq_len(max_items)], collapse = "-"),
      "-plus",
      length(values) - max_items,
      "more"
    )
  } else {
    slug <- paste(values, collapse = "-")
  }
  
  slug <- gsub("\\|", "-", slug)
  slug <- gsub("[^A-Za-z0-9_-]+", "_", slug)
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^-|-$", "", slug)
  slug <- gsub("^_|_$", "", slug)
  
  slug
}


make_actnow_plot_filename <- function(
    plot_type,
    data,
    output_dir = ".",
    extension = "png"
)
{
  if (missing(data) || is.null(data) || nrow(data) == 0) {
    stop("Cannot create plot filename from empty data.")
  }
  
  get_slug <- function(column_name)
  {
    if (!column_name %in% names(data)) {
      return(NULL)
    }
    
    make_actnow_slug(data[[column_name]])
  }
  
  parts <- c(
    make_actnow_slug(plot_type),
    get_slug("indicator"),
    get_slug("scenario"),
    get_slug("model_abbreviation"),
    get_slug("product"),
    get_slug("cadence")
  )
  
  parts <- parts[!is.na(parts)]
  parts <- parts[nchar(parts) > 0]
  
  filename <- paste(parts, collapse = "__")
  filename <- paste0(filename, ".", extension)
  
  file.path(output_dir, filename)
}


