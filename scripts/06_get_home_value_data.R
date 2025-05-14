library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(lubridate)
library(readxl)
library(stringr)

marion_zips <- as.character(c(
  46201, 46202, 46203, 46204, 46205, 46208, 46214, 46217, 46218, 46219, 46220, 46221,
  46222, 46224, 46225, 46226, 46227, 46228, 46229, 46231, 46234, 46235, 46237, 46239,
  46241, 46254, 46256, 46260, 46268, 46278, 46280
))

zhvi_raw <- read_csv("C:/Users/ramra/OneDrive/Desktop/Individual Gentrification Project/gentrification-risk-index/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

zhvi_filtered <- zhvi_raw %>%
  filter(RegionName %in% marion_zips)

zhvi_long <- zhvi_filtered %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "month",
    values_to = "zhvi"
  ) %>%
  mutate(
    month = mdy(month),
    year = year(month)
  ) %>%
  filter(year >= 2015 & year <= 2023)

zhvi_yearly <- zhvi_long %>%
  group_by(RegionName, year) %>%
  summarise(zhvi_avg = mean(zhvi, na.rm = TRUE), .groups = "drop") %>%
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

zhvi_by_tract <- zip_tract_crosswalk %>%
  inner_join(zhvi_yearly, by = "zip") %>%
  mutate(weighted_zhvi = zhvi_avg * ratio) %>%
  group_by(tract, year) %>%
  summarise(
    zhvi_avg_weighted = sum(weighted_zhvi, na.rm = TRUE) / sum(ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(GEOID = tract) %>%
  mutate(zhvi_avg_weighted = na_if(zhvi_avg_weighted, 0))

zhvi_by_tract %>%
  group_by(GEOID) %>%
  summarise(all_na = all(is.na(zhvi_avg_weighted))) %>%
  filter(all_na)