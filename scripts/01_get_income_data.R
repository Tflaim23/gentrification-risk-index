library(tidycensus)
library(dplyr)
library(readr)
library(writexl)
readRenviron("~/.Renviron")
years <- 2015:2023
income_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    variables = "B19013_001", 
    state = "IN",
    county = "Marion",
    year = y,
    survey = "acs5"
  ) %>% mutate(year = y)
})
income_data <- bind_rows(income_list)
write_csv(income_data, "data_raw/income_by_tract_raw.csv")
write_xlsx(income_data, "data_clean/income_by_tract.xlsx")