library(dplyr)
library(readr)
library(writexl)

calls <- read_csv("outputs/forecast_311_call_volume_by_tract.csv")

normalized_calls_levels <- calls %>%
  filter(year %in% 2025:2028) %>%
  group_by(year) %>%
  mutate(call_volume_zscore = as.numeric(scale(call_volume))) %>%
  ungroup()

write_csv(normalized_calls_levels, "normalized_outputs/normalized_call_volume_by_tract.csv")
write_xlsx(normalized_calls_levels, "normalized_outputs/normalized_call_volume_by_tract.xlsx")

calls_deltas <- calls %>%
  filter(year %in% 2024:2028) %>%  
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(call_volume_delta = call_volume / lag(call_volume) - 1) %>%
  ungroup()

normalized_calls_deltas <- calls_deltas %>%
  filter(year %in% 2025:2028, !is.na(call_volume_delta)) %>%
  group_by(year) %>%
  mutate(call_volume_delta_zscore = as.numeric(scale(call_volume_delta))) %>%
  ungroup()

write_csv(normalized_calls_deltas, "normalized_outputs/normalized_call_volume_by_tract_deltas.csv")
write_xlsx(normalized_calls_deltas, "normalized_outputs/normalized_call_volume_by_tract_deltas.xlsx")
