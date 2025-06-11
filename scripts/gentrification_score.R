library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(purrr)

paths <- list.files("normalized_outputs/", pattern = "\\.csv$", full.names = TRUE)
zscore_dfs <- paths %>% 
  set_names(~gsub("normalized_|_by_tract.*", "", basename(.))) %>%
  map(read_csv)

z_all <- reduce(zscore_dfs, full_join, by = c("GEOID", "year"))

z_columns <- names(z_all)[grepl("zscore$", names(z_all))]

gri_raw <- z_all %>%
  rowwise() %>%
  mutate(
    z_scores = list(across(all_of(z_columns))),
    z_filtered = list(unlist(z_scores)[!is.na(unlist(z_scores)) & abs(unlist(z_scores)) <= 4]),
    GRI = ifelse(length(z_filtered) > 0, mean(z_filtered), NA_real_),
    n_valid = length(z_filtered)
  ) %>%
  ungroup()

n_valid_cutoff <- quantile(gri_raw$n_valid, probs = 0.20, na.rm = TRUE)

gri_tertiles <- gri_raw %>%
  group_by(year) %>%
  summarize(
    tertile_1 = quantile(GRI, probs = 1/3, na.rm = TRUE),
    tertile_2 = quantile(GRI, probs = 2/3, na.rm = TRUE),
    .groups = "drop"
  )

gri_final <- gri_raw %>%
  left_join(gri_tertiles, by = "year") %>%
  mutate(
    tier = case_when(
      is.na(GRI) ~ NA_character_,
      GRI <= tertile_1 ~ "Cold",
      GRI <= tertile_2 ~ "Medium",
      TRUE ~ "Hot"
    ),
    low_confidence = n_valid < n_valid_cutoff
  ) %>%
  select(GEOID, year, GRI, n_valid, tier, low_confidence)

gri_final_with_lag <- gri_final %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(
    GRI_lag = lag(GRI),
    GRI_delta = GRI - GRI_lag
  ) %>%
  ungroup()

gri_final_with_lag <- gri_final_with_lag %>%
  group_by(year) %>%
  mutate(
    GRI_delta_z = scale(GRI_delta)
  ) %>%
  ungroup()

gri_final_with_lag <- gri_final_with_lag %>%
  mutate(
    delta_tier = case_when(
      is.na(GRI_delta_z) ~ NA_character_,
      GRI_delta_z <= -1 ~ "Cooling Fast",
      GRI_delta_z >= 1 ~ "Heating Fast",
      TRUE ~ "Stable"
    )
  )

gri_export <- gri_final_with_lag %>%
  select(-starts_with("z_scores"), -starts_with("z_filtered"))

gri_export_clean <- gri_export %>%
  select(where(~ !is.list(.) && !is.matrix(.)))


write_csv(gri_export_clean, "outputs/gentrification_risk_values.csv")
write_xlsx(gri_export_clean, "outputs/gentrification_risk_values.xlsx")
