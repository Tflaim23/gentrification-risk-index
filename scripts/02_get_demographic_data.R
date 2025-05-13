library(tidycensus)
library(dplyr)
library(readr)
library(writexl)

years <- 2015:2023

variables <- c(
  total_pop = "B03002_001",
  white_pop = "B03002_003"
)

demographics_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    variables = variables,
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
      total_pop = total_popE,
      white_pop = white_popE,
      percent_white = 100 * white_pop / total_pop
    )
})

demographics_data <- bind_rows(demographics_list)

write_csv(demographics_data, "data_raw/percent_white_by_tract_raw.csv")
write_xlsx(demographics_data, "data_clean/percent_white_by_tract.xlsx")
