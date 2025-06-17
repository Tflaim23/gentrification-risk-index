library(readr)
library(dplyr)
library(fs)

dir_create("data_standardized_for_interpolation")

files <- list(
  "education_by_tract_raw.csv"      = "percent_bachelors_plus",
  "housing_tenure_burden_raw.csv"   = "percent_rent_burdened",
  "income_by_tract_raw.csv"         = "estimate",
  "mac_calls_by_tract_raw.csv"      = "call_volume",
  "percent_white_by_tract_raw.csv"  = "percent_white",
  "vacancy_rate_by_tract_raw.csv"   = "vacancy_rate",
  "zhvi_by_tract_year_clean.csv"    = "zhvi_avg_weighted",
  "zori_by_tract_year_clean.csv"    = "zori_avg_weighted"
)

for (file in names(files)) {
  path <- file.path("data_raw", file)
  col  <- files[[file]]
  
  if (!file_exists(path)) {
    warning("File does not exist: ", path)
    next
  }
  
  df <- read_csv(path, show_col_types = FALSE)
  if (!(col %in% names(df))) {
    warning("Missing column '", col, "' in ", file)
    next
  }
  
  standardized <- df %>%
    select(
      GEOID,
      NAME  = any_of(c("NAME", "name")),
      year,
      value = all_of(col)
    )
  
  write_csv(standardized, file.path("data_standardized_for_interpolation", file))
}

dir_create("data_raw_2023_interpolation")

std_files <- list.files("data_standardized_for_interpolation", full.names = TRUE)

interpolate_2023 <- function(df) {
  has_name <- "NAME" %in% names(df)
  
  missing_2023 <- df %>%
    group_by(GEOID) %>%
    summarize(has_2023 = any(year == 2023), .groups = "drop") %>%
    filter(!has_2023)
  
  interp_rows <- missing_2023$GEOID %>%
    lapply(function(tract) {
      sub <- df %>% filter(GEOID == tract & !is.na(value)) %>% arrange(year)
      if (nrow(sub) < 2) {
        if (nrow(sub) == 1) {
          return(tibble(
            GEOID = tract,
            NAME  = if (has_name) sub$NAME else NA_character_,
            year  = 2023,
            value = sub$value
          ))
        }
        return(NULL)
      }
      
      earliest   <- sub[1, ]
      latest     <- sub[nrow(sub), ]
      span_years <- latest$year - earliest$year
      
      val_2023 <- if (span_years == 0) {
        latest$value
      } else {
        slope <- (latest$value - earliest$value) / span_years
        pmax(0, latest$value + slope * (2023 - latest$year))
      }
      
      tibble(
        GEOID = tract,
        NAME  = if (has_name) latest$NAME else NA_character_,
        year  = 2023,
        value = val_2023
      )
    }) %>% bind_rows()
  
  df_combined <- df %>%
    bind_rows(interp_rows) %>%
    arrange(GEOID, year)
  
  if (nrow(interp_rows) == 0) {
    message("No 2023 rows interpolated for this dataset.")
  }
  
  return(df_combined)
}

for (file in std_files) {
  message("Processing: ", basename(file))
  df        <- read_csv(file, show_col_types = FALSE)
  df_interp <- interpolate_2023(df)
  out_path  <- file.path("data_raw_2023_interpolation", basename(file))
  message("Writing: ", out_path)
  write_csv(df_interp, out_path)
}