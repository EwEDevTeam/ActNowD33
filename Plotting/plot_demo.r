library(yaml)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

metadata <- read_yaml("actnow_metadata.yaml")

models <- bind_rows(metadata$models)
products <- bind_rows(metadata$products)
scenarios <- bind_rows(metadata$scenarios)
indicators <- bind_rows(metadata$indicators)

select_one <- function(items, id) {
  items %>% filter(.data$id == !!id) %>% slice(1)
}

read_series <- function(model_id, product_id, scenario_id, indicator_id) {
  model <- select_one(models, model_id)

  cadence_preference <- unlist(metadata$cadence_preference)

  for (cadence in cadence_preference) {
    path <- file.path(
      model$root,
      product_id,
      scenario_id,
      cadence,
      paste0(indicator_id, ".csv")
    )

    if (file.exists(path)) {
      df <- read_csv(path, show_col_types = FALSE)

      df <- df %>%
        mutate(
          date = as.Date(date),
          model = model_id,
          product = product_id,
          scenario = scenario_id,
          indicator = indicator_id,
          cadence = cadence
        )

      return(df)
    }
  }

  warning("No file found for: ",
          model_id, " / ", product_id, " / ",
          scenario_id, " / ", indicator_id)

  NULL
}

df <- read_series(
  model_id = "cs05_north-sea_ewe-ecospace",
  product_id = "relative_historical",
  scenario_id = "gs",
  indicator_id = "consumer_biomass"
)

indicator_meta <- select_one(indicators, "consumer_biomass")
product_meta <- select_one(products, "relative_historical")
scenario_meta <- select_one(scenarios, "gs")
model_meta <- select_one(models, "cs05_north-sea_ewe-ecospace")

ggplot(df, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = paste(
      model_meta$label,
      "-",
      indicator_meta$label,
      "-",
      scenario_meta$label
    ),
    subtitle = product_meta$label,
    x = NULL,
    y = "% change"
  ) +
  theme_minimal()

