library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_white <- read_csv("outputs/forecast_percent_white_by_tract.csv")
white_raw <- read_csv("data_raw/percent_white_by_tract_raw.csv") %>%
  filter(year == 2023) %>%
  select(GEOID, NAME, year, percent_white)

combined_data <- bind_rows(white_raw, forecast_white) %>%
  arrange(GEOID, year)

normalized_levels <- combined_data %>%
  filter(year %in% 2024:2028) %>%
  group_by(year) %>%
  mutate(white_zscore = as.numeric(scale(percent_white))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_white_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_white_by_tract.xlsx")

delta_data <- combined_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(white_delta = percent_white / lag(percent_white) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2024:2028, !is.na(white_delta)) %>%
  group_by(year) %>%
  mutate(white_delta_zscore = as.numeric(scale(white_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_white_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_white_by_tract_deltas.xlsx")
