library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(purrr)
library(tidyr)
library(fs)

z_threshold <- 3.99

load_2023_zscore_outliers <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  z_col <- names(df)[str_detect(names(df), "zscore$|_zscore$")]
  if (length(z_col) != 1) return(NULL)
  indicator <- str_remove(basename(file), "zscore_2023_|\\.csv") |> str_replace_all("_raw", "")
  df %>%
    filter(abs(.data[[z_col]]) > z_threshold) %>%
    mutate(indicator = indicator, zscore = .data[[z_col]], year = 2023) %>%
    select(GEOID, year, indicator, zscore)
}

load_forecast_or_delta_outliers <- function(file, z_col_pattern = "_zscore$") {
  df <- read_csv(file, show_col_types = FALSE)
  z_col <- names(df)[str_detect(names(df), z_col_pattern)]
  if (length(z_col) != 1) return(NULL)
  indicator <- str_remove(basename(file), "normalized_|_by_tract.*|_deltas")
  df %>%
    filter(abs(.data[[z_col]]) > z_threshold) %>%
    mutate(indicator = indicator, zscore = .data[[z_col]]) %>%
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

all_outliers <- bind_rows(
  outliers_2023 %>% mutate(type = "baseline"),
  outliers_forecast %>% mutate(type = "forecast"),
  outliers_delta %>% mutate(type = "delta")
)

dir_create("final_score_table")
write_xlsx(all_outliers, "final_score_table/all_extreme_zscores_combined.xlsx")

raw_files <- list(
  education = list(file = "data_raw/education_by_tract_raw.csv",        col = "percent_bachelors_plus"),
  housing   = list(file = "data_raw/housing_tenure_burden_raw.csv",     col = "percent_rent_burdened"),
  income    = list(file = "data_raw/income_by_tract_raw.csv",           col = "estimate"),
  calls     = list(file = "data_raw/mac_calls_by_tract_raw.csv",        col = "call_volume"),
  white     = list(file = "data_raw/percent_white_by_tract_raw.csv",    col = "percent_white"),
  vacancy   = list(file = "data_raw/vacancy_rate_by_tract_raw.csv",     col = "vacancy_rate"),
  zhvi      = list(file = "data_raw/zhvi_by_tract_year_clean.csv",      col = "zhvi_avg_weighted"),
  zori      = list(file = "data_raw/zori_by_tract_year_clean.csv",      col = "zori_avg_weighted")
)

raw_long <- imap_dfr(raw_files, function(file_info, indicator) {
  read_csv(file_info$file, show_col_types = FALSE) %>%
    select(GEOID, year, value = all_of(file_info$col)) %>%
    mutate(indicator = indicator)
})

raw_wide <- raw_long %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  arrange(GEOID, year)

write_xlsx(raw_wide, "final_score_table/raw_data_all_indicators.xlsx")
