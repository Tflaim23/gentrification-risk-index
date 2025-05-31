library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(writexl)
library(tidyr)
library(feasts)
library(purrr)
library(fabletools)

income_data <- read_csv("data_raw/income_by_tract_raw.csv")

income_ts <- income_data %>%
  filter(!is.na(estimate), !is.na(moe)) %>%
  filter(moe / estimate <= 0.5, estimate > 0) %>%
  select(GEOID, NAME, year, estimate) %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  mutate(log_estimate = log(estimate)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

validate_forecast <- function(log_values) {
  values <- exp(log_values)
  if (any(is.na(values)) || length(values) < 2) return(FALSE)
  max_growth <- max(values[-1] / values[-length(values)], na.rm = TRUE)
  return(max_growth <= 1.5)
}

fit_arima_safely <- function(ts_data) {
  full_model <- tryCatch({
    model(ts_data, arima = ARIMA(log_estimate ~ trend() + pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) NULL)
  
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
  tryCatch({
    model(ts_data, arima = ARIMA(log_estimate ~ pdq(1, 1, 0)))
  }, error = function(e2) NULL)
}


income_models <- income_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

income_forecast <- income_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

income_forecast_clean <- income_forecast %>%
  filter(!is.na(.mean)) %>%
  mutate(estimate = exp(.mean)) %>%
  select(GEOID, year, estimate) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, estimate)

write_csv(income_forecast_clean, "outputs/forecast_income_by_tract.csv")
write_xlsx(income_forecast_clean, "outputs/forecast_income_by_tract.xlsx")

model_orders <- income_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))