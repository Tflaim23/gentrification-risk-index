tracts_to_remove <- c("18063210607", "18063210608", "18063210614", 
                      "18097342000", "18097356900", "18097358000")
#based_on_output_of_first_script

zori_by_tract_clean <- zori_by_tract %>%
  filter(!GEOID %in% tracts_to_remove)

write_csv(zori_by_tract_clean, "data_raw/zori_by_tract_year_clean.csv")
write_xlsx(zori_by_tract_clean, "data_clean/zori_by_tract_year_clean.xlsx")
