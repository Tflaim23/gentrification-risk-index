library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_income <- read_csv("outputs/forecast_income_by_tract.csv")

normalized_levels <- forecast_income %>%
  filter(year %in% 2025:2028) %>%
  group_by(year) %>%
  mutate(income_zscore = as.numeric(scale(estimate))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_income_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_income_by_tract.xlsx")

delta_data <- forecast_income %>%
  filter(year %in% 2024:2028) %>%  
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(income_delta = estimate / lag(estimate) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2025:2028, !is.na(income_delta)) %>%
  group_by(year) %>%
  mutate(income_delta_zscore = as.numeric(scale(income_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_income_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_income_by_tract_deltas.xlsx")

