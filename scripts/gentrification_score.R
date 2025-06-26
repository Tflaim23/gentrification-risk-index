library(readr)
library(dplyr)
library(stringr)
library(writexl)

load_2023_zscore_wide <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  zscore_col <- names(df)[str_detect(names(df), "zscore$|_zscore$")]
  if (length(zscore_col) != 1) return(NULL)
  
  indicator <- str_remove(basename(file), "zscore_2023_|\\.csv") |> str_replace_all("_raw", "")
  df %>%
    select(GEOID, !!paste0(indicator, "_zscore") := all_of(zscore_col))
}

load_forecast_or_delta_wide <- function(file, z_col_pattern = "_zscore$") {
  df <- read_csv(file, show_col_types = FALSE)
  zscore_col <- names(df)[str_detect(names(df), z_col_pattern)]
  if (length(zscore_col) != 1) return(NULL)
  
  indicator <- str_remove(basename(file), "normalized_|_by_tract.*|_deltas")
  df %>%
    select(GEOID, year, !!paste0(indicator, "_zscore") := all_of(zscore_col))
}

zscore_2023_files <- list.files("2023_zscore", pattern = "\\.csv$", full.names = TRUE)
zscore_2023_use <- zscore_2023_files[!str_detect(zscore_2023_files, "call_volume|pressure")]

z_2023_list <- lapply(zscore_2023_use, load_2023_zscore_wide)
z_2023_wide <- Reduce(function(x, y) full_join(x, y, by = "GEOID"), z_2023_list)

write_xlsx(z_2023_wide, "final_score_table/zscore_2023_wide.xlsx")

forecast_files <- list.files("normalized_outputs", pattern = "^normalized_.*\\.csv$", full.names = TRUE)
forecast_files <- forecast_files[!str_detect(forecast_files, "_deltas")]

forecast_list <- lapply(forecast_files, function(f) load_forecast_or_delta_wide(f, "_zscore$"))
forecast_wide <- Reduce(function(x, y) full_join(x, y, by = c("GEOID", "year")), forecast_list)

write_xlsx(forecast_wide, "final_score_table/forecast_zscores_wide.xlsx")

delta_files <- list.files("normalized_outputs", pattern = "_deltas\\.csv$", full.names = TRUE)
delta_list <- lapply(delta_files, function(f) load_forecast_or_delta_wide(f, "_delta_zscore$"))
delta_wide <- Reduce(function(x, y) full_join(x, y, by = c("GEOID", "year")), delta_list)

write_xlsx(delta_wide, "final_score_table/delta_zscores_wide.xlsx")
