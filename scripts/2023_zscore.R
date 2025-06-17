library(dplyr)
library(readr)
library(writexl)
library(fs)

dir_create("2023_zscore")

files <- list.files("data_raw_2023_interpolation", pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("mac_calls", files)]

for (path in files) {
  indicator <- gsub("_by_tract.*|_year_clean.*|data_raw_2023_interpolation/", "", path)
  
  df <- read_csv(path, show_col_types = FALSE)
  df_2023 <- df %>% filter(year == 2023)
  
  if ("NAME" %in% names(df_2023)) {
    df_2023 <- df_2023 %>% select(GEOID, NAME, value)
  } else {
    df_2023 <- df_2023 %>% select(GEOID, value)
  }
  
  df_2023 <- df_2023 %>%
    mutate(
      zscore = as.numeric(scale(value)),
      zscore = if (grepl("vacancy", indicator)) -1 * zscore else zscore
    )
  
  out_csv  <- file.path("2023_zscore", paste0("zscore_2023_", indicator, ".csv"))
  out_xlsx <- file.path("2023_zscore", paste0("zscore_2023_", indicator, ".xlsx"))
  
  write_csv(df_2023, out_csv)
  write_xlsx(df_2023, out_xlsx)
}

