library(dplyr)
library(readr)
library(lubridate)
library(readxl)
library(writexl)
library(stringr)

marion_zips <- as.character(c(
  46201, 46202, 46203, 46204, 46205, 46208, 46214, 46217, 46218, 46219, 46220, 46221,
  46222, 46224, 46225, 46226, 46227, 46228, 46229, 46231, 46234, 46235, 46237, 46239,
  46241, 46254, 46256, 46260, 46268, 46278, 46280
))

legacy_311 <- read_csv(
  "C:/Users/ramra/OneDrive/Desktop/Individual Gentrification Project/gentrification-risk-index/Mayor_s_Action_Center_Service_Cases.csv"
)

legacy_filtered <- legacy_311 %>%
  filter(ZIP__C %in% as.integer(marion_zips)) %>%
  mutate(
    requested_date = ymd_hms(CREATEDDATE, quiet = TRUE),
    year = year(requested_date),
    zip = str_pad(as.character(ZIP__C), width = 5, pad = "0")
  ) %>%
  filter(year >= 2015, year <= 2023)

crosswalk <- read_excel(
  "C:/Users/ramra/OneDrive/Desktop/Individual Gentrification Project/gentrification-risk-index/ZIP_TRACT_032023.xlsx"
)

zip_tract_crosswalk <- crosswalk %>%
  mutate(
    zip = str_pad(as.character(ZIP), 5, pad = "0"),
    tract = str_pad(as.character(TRACT), 11, pad = "0"),
    county = str_sub(tract, 1, 5)  # first 5 digits of GEOID = state + county
  ) %>%
  filter(zip %in% marion_zips, county == "18097") %>%
  transmute(zip, tract, ratio = RES_RATIO)

legacy_weighted <- legacy_filtered %>%
  left_join(zip_tract_crosswalk, by = "zip") %>%
  filter(!is.na(tract), !is.na(ratio)) %>%
  group_by(tract, year) %>%
  summarise(call_volume = sum(ratio, na.rm = TRUE), .groups = "drop") %>%
  rename(GEOID = tract)

legacy_weighted <- legacy_weighted %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, call_volume)

write_csv(legacy_weighted, "data_raw/mac_calls_by_tract_raw.csv")
write_xlsx(legacy_weighted, "data_clean/mac_calls_by_tract.xlsx")

