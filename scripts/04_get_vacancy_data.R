library(tidycensus)
library(dplyr)
library(readr)
library(writexl)

years <- 2015:2023

vacancy_vars <- c(
  total_units = "B25002_001",
  vacant_units = "B25002_003"
)

vacancy_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    variables = vacancy_vars,
    state = "IN",
    county = "Marion",
    year = y,
    survey = "acs5",
    output = "wide"
  ) %>%
    transmute(
      GEOID,
      NAME,
      year = y,
      total_units = total_unitsE,
      vacant_units = vacant_unitsE,
      vacancy_rate = 100 * vacant_units / total_units
    )
})

vacancy_data <- bind_rows(vacancy_list)

write_csv(vacancy_data, "data_raw/vacancy_rate_by_tract_raw.csv")
write_xlsx(vacancy_data, "data_clean/vacancy_rate_by_tract.xlsx")
