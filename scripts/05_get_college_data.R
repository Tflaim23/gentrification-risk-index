library(tidycensus)
library(dplyr)
library(readr)
library(writexl)

years <- 2015:2023

edu_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    variables = c(
      total = "B15003_001",
      bachelors = "B15003_022",
      masters = "B15003_023",
      professional = "B15003_024",
      doctorate = "B15003_025"
    ),
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
      total_25plus = totalE,
      bachelors_plus = bachelorsE + mastersE + professionalE + doctorateE,
      percent_bachelors_plus = 100 * bachelors_plus / total_25plus
    )
})

edu_data <- bind_rows(edu_list)

write_csv(edu_data, "data_raw/education_by_tract_raw.csv")
write_xlsx(edu_data, "data_clean/education_by_tract.xlsx")
