library(dplyr)
library(readr)
library(writexl)

housing <- read_csv("outputs/forecast_housing_pressure_by_tract.csv")

normalized_housing_levels <- housing %>%
  filter(year %in% 2025:2028) %>%
  group_by(year) %>%
  mutate(housing_pressure_zscore = as.numeric(scale(housing_pressure_forecast))) %>%
  ungroup()

write_csv(normalized_housing_levels, "normalized_outputs/normalized_housing_pressure_by_tract.csv")
write_xlsx(normalized_housing_levels, "normalized_outputs/normalized_housing_pressure_by_tract.xlsx")

housing_deltas <- housing %>%
  filter(year %in% 2024:2028) %>%  
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(housing_pressure_delta = housing_pressure_forecast / lag(housing_pressure_forecast) - 1) %>%
  ungroup()

normalized_housing_deltas <- housing_deltas %>%
  filter(year %in% 2025:2028, !is.na(housing_pressure_delta)) %>%
  group_by(year) %>%
  mutate(housing_pressure_delta_zscore = as.numeric(scale(housing_pressure_delta))) %>%
  ungroup()

write_csv(normalized_housing_deltas, "normalized_outputs/normalized_housing_pressure_by_tract_deltas.csv")
write_xlsx(normalized_housing_deltas, "normalized_outputs/normalized_housing_pressure_by_tract_deltas.xlsx")
