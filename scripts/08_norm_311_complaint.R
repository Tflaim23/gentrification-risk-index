library(dplyr)
library(readr)
library(writexl)
library(purrr)

calls_forecast <- read_csv("outputs/forecast_311_call_volume_by_tract.csv")

calls_raw <- read_csv("data_raw_2023_interpolation/mac_calls_by_tract_raw.csv") %>%
  filter(year == 2023) %>%
  select(GEOID, NAME, year, call_volume = value)

combined_calls <- bind_rows(calls_raw, calls_forecast) %>%
  arrange(GEOID, year)

income_z <- read_csv("2023_zscore/zscore_2023_income.csv") %>%
  select(GEOID, income_z = zscore)

white_z <- read_csv("2023_zscore/zscore_2023_percent_white.csv") %>%
  select(GEOID, white_z = zscore)

zori_z <- read_csv("2023_zscore/zscore_2023_zori.csv") %>%
  select(GEOID, zori_z = zscore)

stage_info <- list(income_z, white_z, zori_z) %>%
  reduce(full_join, by = "GEOID") %>%
  rowwise() %>%
  mutate(
    score_vector = list(c_across(c(income_z, white_z, zori_z))),
    stage_score = mean(unlist(score_vector), na.rm = TRUE),
    valid_n = sum(!is.na(unlist(score_vector))),
    stage_weight = case_when(
      is.na(stage_score) ~ NA_character_,
      stage_score >= 1.5 ~ "super_late",
      stage_score >= 0.5 ~ "late",
      stage_score <= -1.5 ~ "super_early",
      stage_score <= -0.5 ~ "early",
      TRUE ~ "middle"
    )
  ) %>%
  ungroup() %>%
  select(GEOID, income_z, white_z, zori_z, stage_score, valid_n, stage_weight)

normalized_calls_levels <- combined_calls %>%
  filter(year %in% 2024:2028) %>%
  group_by(year) %>%
  mutate(call_volume_zscore_raw = as.numeric(scale(call_volume))) %>%
  ungroup() %>%
  left_join(stage_info, by = "GEOID") %>%
  mutate(
    call_volume_zscore_adj_raw = case_when(
      stage_weight == "super_late"   ~ -2 * call_volume_zscore_raw,
      stage_weight == "late"         ~ -1 * call_volume_zscore_raw,
      stage_weight == "middle"       ~  0,
      stage_weight == "early"        ~  1 * call_volume_zscore_raw,
      stage_weight == "super_early"  ~  2 * call_volume_zscore_raw,
      TRUE                           ~ call_volume_zscore_raw
    ),
    call_volume_zscore = pmin(pmax(call_volume_zscore_adj_raw, -4), 4)
  )

write_csv(normalized_calls_levels, "normalized_outputs/normalized_call_volume_by_tract.csv")
write_xlsx(normalized_calls_levels, "normalized_outputs/normalized_call_volume_by_tract.xlsx")

calls_deltas <- combined_calls %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(call_volume_delta = call_volume / lag(call_volume) - 1) %>%
  ungroup()

normalized_calls_deltas <- calls_deltas %>%
  filter(year %in% 2024:2028, !is.na(call_volume_delta)) %>%
  group_by(year) %>%
  mutate(call_volume_delta_zscore_raw = as.numeric(scale(call_volume_delta))) %>%
  ungroup() %>%
  left_join(stage_info, by = "GEOID") %>%
  mutate(
    call_volume_delta_zscore_adj_raw = case_when(
      stage_weight == "super_late"   ~ -2 * call_volume_delta_zscore_raw,
      stage_weight == "late"         ~ -1 * call_volume_delta_zscore_raw,
      stage_weight == "middle"       ~  0,
      stage_weight == "early"        ~  1 * call_volume_delta_zscore_raw,
      stage_weight == "super_early"  ~  2 * call_volume_delta_zscore_raw,
      TRUE                           ~ call_volume_delta_zscore_raw
    ),
    call_volume_delta_zscore = pmin(pmax(call_volume_delta_zscore_adj_raw, -4), 4)
  )

write_csv(normalized_calls_deltas, "normalized_outputs/normalized_call_volume_by_tract_deltas.csv")
write_xlsx(normalized_calls_deltas, "normalized_outputs/normalized_call_volume_by_tract_deltas.xlsx")
