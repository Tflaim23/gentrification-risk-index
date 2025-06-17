library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_zori <- read_csv("outputs/forecast_zori_by_tract.csv")

zori_raw <- read_csv("data_raw/zori_by_tract_year_clean.csv") %>%
  filter(year == 2023) %>%
  transmute(
    GEOID,
    year,
    zori_forecast = zori_avg_weighted
  )

combined_data <- bind_rows(zori_raw, forecast_zori) %>%
  arrange(GEOID, year)

normalized_levels <- combined_data %>%
  filter(year %in% 2024:2028) %>%
  group_by(year) %>%
  mutate(zori_zscore = as.numeric(scale(zori_forecast))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_zori_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_zori_by_tract.xlsx")

delta_data <- combined_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(zori_delta = zori_forecast / lag(zori_forecast) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2024:2028, !is.na(zori_delta)) %>%
  group_by(year) %>%
  mutate(zori_delta_zscore = as.numeric(scale(zori_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_zori_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_zori_by_tract_deltas.xlsx")
