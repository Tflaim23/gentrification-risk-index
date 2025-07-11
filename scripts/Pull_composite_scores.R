library(readxl)
library(dplyr)
library(scales)
library(writexl)

path <- "gentrification_risk_index.xlsm"
sheet <- "Composite Gentrification Score"

raw <- read_excel(path, sheet = sheet, range = "B1:H5000", col_names = FALSE)
names(raw) <- c("GEOID", "year", "skip1", "baseline", "skip2", "skip3", "composite")
data <- raw %>% select(GEOID, year, baseline, composite) %>%
  filter(!is.na(GEOID) & !is.na(year) & !is.na(composite))

data <- data %>%
  group_by(year) %>%
  mutate(composite_zscore = as.numeric(scale(composite))) %>%
  ungroup()

baseline_percentiles <- quantile(data$baseline, probs = c(0.1, 0.3, 0.7, 0.9), na.rm = TRUE)

data <- data %>%
  mutate(
    baseline_category = case_when(
      baseline <= baseline_percentiles[1] ~ "Highly gentrified",
      baseline <= baseline_percentiles[2] ~ "Somewhat gentrified",
      baseline <= baseline_percentiles[3] ~ "Moderate",
      baseline <= baseline_percentiles[4] ~ "Very low gentrification",
      TRUE ~ "No gentrification"
    )
  )

write_csv(data, "final_outputs/final_gentrification_scores.csv")
write_xlsx(data, "final_outputs/final_gentrification_scores.xlsx")
