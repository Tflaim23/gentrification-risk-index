library(tidycensus)
library(dplyr)
library(readr)
library(writexl)

years <- 2015:2023

tenure_burden_list <- lapply(years, function(y) {
  tenure_data <- get_acs(
    geography = "tract",
    variables = c(
      total_units = "B25003_001",
      owner_occupied = "B25003_002",
      renter_occupied = "B25003_003"
    ),
    state = "IN",
    county = "Marion",
    year = y,
    survey = "acs5",
    output = "wide"
  )
  
  burden_data <- get_acs(
    geography = "tract",
    variables = c(
      total_renters = "B25070_001",
      rent_30_34 = "B25070_007",
      rent_35_39 = "B25070_008",
      rent_40_49 = "B25070_009",
      rent_50_plus = "B25070_010"
    ),
    state = "IN",
    county = "Marion",
    year = y,
    survey = "acs5",
    output = "wide"
  )
  
  combined <- tenure_data %>%
    transmute(
      GEOID,
      NAME,
      year = y,
      total_units = total_unitsE,
      owner_occupied = owner_occupiedE,
      renter_occupied = renter_occupiedE,
      owner_occupancy_rate = 100 * owner_occupied / total_units,
      renter_occupancy_rate = 100 * renter_occupied / total_units
    ) %>%
    left_join(
      burden_data %>%
        transmute(
          GEOID,
          rent_burdened = rent_30_34E + rent_35_39E + rent_40_49E + rent_50_plusE,
          total_renters = total_rentersE,
          percent_rent_burdened = 100 * rent_burdened / total_renters
        ),
      by = "GEOID"
    )
  
  return(combined)
})

tenure_burden_data <- bind_rows(tenure_burden_list)

write_csv(tenure_burden_data, "data_raw/housing_tenure_burden_raw.csv")
write_xlsx(tenure_burden_data, "data_clean/housing_tenure_burden.xlsx")
