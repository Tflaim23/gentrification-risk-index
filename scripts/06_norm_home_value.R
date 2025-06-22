library(dplyr)
library(readr)
library(writexl)

zhvi_forecast <- read_csv("outputs/forecast_zhvi_by_tract.csv")

zhvi_2023 <- read_csv("data_raw_2023_interpolation/zhvi_by_tract_year_clean.csv") %>%
  filter(year == 2023) %>%
  rename(zhvi_forecast = value) %>%
  select(GEOID, year, zhvi_forecast)

zhvi <- bind_rows(zhvi_2023, zhvi_forecast) %>%
  arrange(GEOID, year)

normalized_zhvi_levels <- zhvi %>%
  filter(year %in% 2025:2028) %>%
  group_by(year) %>%
  mutate(zhvi_zscore = as.numeric(scale(zhvi_forecast))) %>%
  ungroup()

write_csv(normalized_zhvi_levels, "normalized_outputs/normalized_zhvi_by_tract.csv")
write_xlsx(normalized_zhvi_levels, "normalized_outputs/normalized_zhvi_by_tract.xlsx")

zhvi_deltas <- zhvi %>%
  filter(year %in% 2024:2028) %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(zhvi_delta = zhvi_forecast / lag(zhvi_forecast) - 1) %>%
  ungroup()

normalized_zhvi_deltas <- zhvi_deltas %>%
  filter(year %in% 2025:2028, !is.na(zhvi_delta)) %>%
  group_by(year) %>%
  mutate(zhvi_delta_zscore = as.numeric(scale(zhvi_delta))) %>%
  ungroup()

write_csv(normalized_zhvi_deltas, "normalized_outputs/normalized_zhvi_by_tract_deltas.csv")
write_xlsx(normalized_zhvi_deltas, "normalized_outputs/normalized_zhvi_by_tract_deltas.xlsx")
