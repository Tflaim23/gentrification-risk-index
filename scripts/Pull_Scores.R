library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(readr)

final_scores <- read_excel("gentrification_risk_index.xlsm", sheet = "Final Score") %>%
  select(GEOID, Year, Score, Confidence) %>%
  filter(!is.na(Score) & Score != "Lacking good data") %>%
  mutate(
    Score = as.numeric(Score),
    Year = as.integer(Year)
  )

normalized_scores <- final_scores %>%
  group_by(Year) %>%
  mutate(
    raw_z = as.vector(scale(Score)),
    normalized_score = pmin(pmax(raw_z, -4), 4)
  ) %>%
  ungroup()

normalized_scores <- normalized_scores %>%
  arrange(GEOID, Year) %>%
  group_by(GEOID) %>%
  mutate(cumulative_score = cumsum(normalized_score)) %>%
  ungroup()

final_out <- read_excel("gentrification_risk_index.xlsm", sheet = "Final Score") %>%
  select(GEOID, Year, Score, Confidence) %>%
  left_join(
    normalized_scores %>% select(GEOID, Year, normalized_score, cumulative_score),
    by = c("GEOID", "Year")
  )

write_csv(final_out, "final_outputs/normalized_final_score.csv")
write_xlsx(final_out, "final_outputs/normalized_final_score.xlsx")
