library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_edu <- read_csv("outputs/forecast_education_by_tract.csv")

normalized_levels <- forecast_edu %>%
  filter(year %in% 2025:2028) %>%
  group_by(year) %>%
  mutate(edu_zscore = as.numeric(scale(percent_bachelors_plus))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_education_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_education_by_tract.xlsx")

delta_data <- forecast_edu %>%
  filter(year %in% 2024:2028) %>%  
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(edu_delta = percent_bachelors_plus / lag(percent_bachelors_plus) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2025:2028, !is.na(edu_delta)) %>%
  group_by(year) %>%
  mutate(edu_delta_zscore = as.numeric(scale(edu_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_education_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_education_by_tract_deltas.xlsx")
