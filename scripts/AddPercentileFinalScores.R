library(dplyr)
library(readr)
library(writexl)

final_out <- read_csv("final_outputs/normalized_final_score.csv")

final_out_clean <- final_out %>%
  filter(!is.na(normalized_score), !is.na(cumulative_score)) %>%
  mutate(
    normalized_score = as.numeric(normalized_score),
    cumulative_score = as.numeric(cumulative_score),
    Year = as.integer(Year)
  )

final_out_percentiles <- final_out_clean %>%
  group_by(Year) %>%
  mutate(
    normalized_percentile = round(100 * percent_rank(normalized_score), 1),
    cumulative_percentile = round(100 * percent_rank(cumulative_score), 1)
  ) %>%
  ungroup()

write_csv(final_out_percentiles, "final_outputs/scores_with_percentiles.csv")
write_xlsx(final_out_percentiles, "final_outputs/scores_with_percentiles.xlsx")
