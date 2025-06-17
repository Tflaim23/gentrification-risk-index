library(dplyr)
library(readr)
library(tidyr)
library(writexl)

forecast_vacancy <- read_csv("outputs/forecast_vacancy_rate_by_tract.csv")

vacancy_raw <- read_csv("data_raw/vacancy_rate_by_tract_raw.csv") %>%
  filter(year == 2023) %>%
  transmute(
    GEOID,
    year,
    vacancy_rate_forecast = vacancy_rate
  )

combined_data <- bind_rows(vacancy_raw, forecast_vacancy) %>%
  arrange(GEOID, year)

normalized_levels <- combined_data %>%
  filter(year %in% 2024:2028) %>%
  group_by(year) %>%
  mutate(vacancy_zscore = -as.numeric(scale(vacancy_rate_forecast))) %>%
  ungroup()

write_csv(normalized_levels, "normalized_outputs/normalized_vacancy_rate_by_tract.csv")
write_xlsx(normalized_levels, "normalized_outputs/normalized_vacancy_rate_by_tract.xlsx")

delta_data <- combined_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(vacancy_delta = vacancy_rate_forecast / lag(vacancy_rate_forecast) - 1) %>%
  ungroup()

normalized_deltas <- delta_data %>%
  filter(year %in% 2024:2028, !is.na(vacancy_delta)) %>%
  group_by(year) %>%
  mutate(vacancy_delta_zscore = -as.numeric(scale(vacancy_delta))) %>%
  ungroup()

write_csv(normalized_deltas, "normalized_outputs/normalized_vacancy_rate_by_tract_deltas.csv")
write_xlsx(normalized_deltas, "normalized_outputs/normalized_vacancy_rate_by_tract_deltas.xlsx")
