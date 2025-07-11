library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(purrr)

z_threshold <- 3.99

load_2023_zscore_outliers <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  z_col <- names(df)[str_detect(names(df), "zscore$|_zscore$")]
  if (length(z_col) != 1) return(NULL)
  
  indicator <- str_remove(basename(file), "zscore_2023_|\\.csv") |> str_replace_all("_raw", "")
  df %>%
    filter(abs(.data[[z_col]]) > z_threshold) %>%
    mutate(
      indicator = indicator,
      zscore = .data[[z_col]]
    ) %>%
    select(GEOID, indicator, zscore)
}

load_forecast_or_delta_outliers <- function(file, z_col_pattern = "_zscore$") {
  df <- read_csv(file, show_col_types = FALSE)
  z_col <- names(df)[str_detect(names(df), z_col_pattern)]
  if (length(z_col) != 1) return(NULL)
  
  indicator <- str_remove(basename(file), "normalized_|_by_tract.*|_deltas")
  df %>%
    filter(abs(.data[[z_col]]) > z_threshold) %>%
    mutate(
      indicator = indicator,
      zscore = .data[[z_col]]
    ) %>%
    select(GEOID, year, indicator, zscore)
}

zscore_2023_files <- list.files("2023_zscore", pattern = "\\.csv$", full.names = TRUE) %>%
  discard(~ str_detect(.x, "call_volume|pressure"))

forecast_files <- list.files("normalized_outputs", pattern = "^normalized_.*\\.csv$", full.names = TRUE) %>%
  discard(~ str_detect(.x, "_deltas"))

delta_files <- list.files("normalized_outputs", pattern = "_deltas\\.csv$", full.names = TRUE)

outliers_2023 <- map_dfr(zscore_2023_files, load_2023_zscore_outliers)
outliers_forecast <- map_dfr(forecast_files, ~load_forecast_or_delta_outliers(.x, "_zscore$"))
outliers_delta <- map_dfr(delta_files, ~load_forecast_or_delta_outliers(.x, "_delta_zscore$"))

write_xlsx(outliers_2023, "final_score_table/extreme_zscores_2023.xlsx")
write_xlsx(outliers_forecast, "final_score_table/extreme_zscores_forecast.xlsx")
write_xlsx(outliers_delta, "final_score_table/extreme_zscores_delta.xlsx")
