library(dplyr)
library(readr)
library(writexl)

forecast_housing <- read_csv("outputs/forecast_housing_pressure_by_tract.csv")

housing_raw <- read_csv("data_raw_2023_interpolation/housing_tenure_burden_raw.csv") %>%
  filter(year == 2023) %>%
  mutate(
    housing_pressure_forecast = value 
  ) %>%
  select(GEOID, NAME, year, housing_pressure_forecast)

combined_data <- bind_rows(housing_raw, forecast_housing) %>%
  arrange(GEOID, year)

normalized_levels <- combined_data %>%
  filter(year >= 2024 & year <= 2028) %>%
  group_by(year) %>%
  mutate(housing_pressure_zscore = as.numeric(scale(housing_pressure_forecast))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_housing_pressure_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_housing_pressure_by_tract.xlsx")

delta_data <- combined_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(housing_pressure_delta = housing_pressure_forecast / lag(housing_pressure_forecast) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year >= 2024 & year <= 2028, !is.na(housing_pressure_delta)) %>%
  group_by(year) %>%
  mutate(housing_pressure_delta_zscore = as.numeric(scale(housing_pressure_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_housing_pressure_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_housing_pressure_by_tract_deltas.xlsx")
