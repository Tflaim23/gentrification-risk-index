library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

tenure_data <- read_csv("data_raw/housing_tenure_burden_raw.csv")

housing_pressure_ts <- tenure_data %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year),
    housing_pressure_score = renter_occupancy_rate * percent_rent_burdened / 100
  ) %>%
  select(GEOID, NAME, year, housing_pressure_score) %>%
  filter(!is.na(housing_pressure_score), housing_pressure_score > 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  mutate(log_score = log(housing_pressure_score)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts_data) {
  tryCatch({
    model(ts_data, arima = ARIMA(log_score ~ pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) {
    message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
    tryCatch({
      model(ts_data, arima = ARIMA(log_score ~ pdq(1, 1, 0)))
    }, error = function(e2) {
      return(NULL)
    })
  })
}

housing_models <- housing_pressure_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

housing_forecast <- housing_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

housing_forecast_clean <- housing_forecast %>%
  filter(!is.na(.mean)) %>%
  mutate(housing_pressure_forecast = exp(.mean)) %>%
  select(GEOID, year, housing_pressure_forecast) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, housing_pressure_forecast)

write_csv(housing_forecast_clean, "outputs/forecast_housing_pressure_by_tract.csv")
write_xlsx(housing_forecast_clean, "outputs/forecast_housing_pressure_by_tract.xlsx")

model_orders <- housing_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
