tracts_to_remove <- c()
#based_on_output_of_first_script

zhvi_by_tract_clean <- zhvi_by_tract %>%
  filter(!GEOID %in% tracts_to_remove)

write_csv(zhvi_by_tract_clean, "data_raw/zhvi_by_tract_year_clean.csv")
write_xlsx(zhvi_by_tract_clean, "data_clean/zhvi_by_tract_year_clean.xlsx")

