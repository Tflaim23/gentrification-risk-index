library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(lubridate)
library(readxl)
library(stringr)

marion_zips <- c(
  46201, 46202, 46203, 46204, 46205, 46208, 46214, 46217, 46218, 46219, 46220, 46221,
  46222, 46224, 46225, 46226, 46227, 46228, 46229, 46231, 46234, 46235, 46237, 46239,
  46241, 46254, 46256, 46260, 46268, 46278, 46280
)

zori_raw <- read_csv("C:/Users/ramra/OneDrive/Desktop/Individual Gentrification Project/gentrification-risk-index/Zip_zori_uc_sfrcondomfr_sm_month.csv")

zori_filtered <- zori_raw %>%
  filter(RegionName %in% marion_zips)

zori_long <- zori_filtered %>%
  pivot_longer(
    cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"), 
    names_to = "month",
    values_to = "zori"
  ) %>%
  mutate(
    month = mdy(month), 
    year = year(month)
  ) %>%
  filter(year >= 2015 & year <= 2023)

zori_yearly <- zori_long %>%
  group_by(RegionName, year) %>%
  summarise(zori_avg = mean(zori, na.rm = TRUE), .groups = "drop") %>%
  rename(zip = RegionName) %>%
  mutate(zip = as.character(zip))

crosswalk <- read_excel("C:/Users/ramra/OneDrive/Desktop/Individual Gentrification Project/gentrification-risk-index/ZIP_TRACT_032023.xlsx")

zip_tract_crosswalk <- crosswalk %>%
  transmute(
    zip = as.character(ZIP),
    tract = str_pad(as.character(TRACT), width = 11, side = "left", pad = "0"),
    ratio = RES_RATIO
  ) %>%
  filter(zip %in% as.character(marion_zips))

zori_by_tract <- zip_tract_crosswalk %>%
  inner_join(zori_yearly, by = "zip") %>%
  mutate(weighted_zori = zori_avg * ratio) %>%
  group_by(tract, year) %>%
  summarise(
    zori_avg_weighted = sum(weighted_zori, na.rm = TRUE) / sum(ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(GEOID = tract) %>%
  mutate(zori_avg_weighted = na_if(zori_avg_weighted, 0))

zori_by_tract %>%
  group_by(GEOID) %>%
  summarise(all_na = all(is.na(zori_avg_weighted))) %>%
  filter(all_na)

#See_Which_Tracts_Lack_Data
