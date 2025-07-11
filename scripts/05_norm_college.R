library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_edu <- read_csv("outputs/forecast_education_by_tract.csv")

edu_raw <- read_csv("data_raw/education_by_tract_raw.csv") %>%
  filter(year == 2023) %>%
  select(GEOID, NAME, year, percent_bachelors_plus)

combined_data <- bind_rows(edu_raw, forecast_edu) %>%
  arrange(GEOID, year)

normalized_levels <- combined_data %>%
  filter(year %in% 2024:2028) %>%
  group_by(year) %>%
  mutate(
    edu_zscore_raw = as.numeric(scale(percent_bachelors_plus)),
    edu_zscore = pmin(pmax(edu_zscore_raw, -4), 4)
  ) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_education_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_education_by_tract.xlsx")

delta_data <- combined_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(edu_delta = percent_bachelors_plus / lag(percent_bachelors_plus) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2024:2028, !is.na(edu_delta)) %>%
  group_by(year) %>%
  mutate(
    edu_delta_zscore_raw = as.numeric(scale(edu_delta)),
    edu_delta_zscore = pmin(pmax(edu_delta_zscore_raw, -4), 4)
  ) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_education_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_education_by_tract_deltas.xlsx")
