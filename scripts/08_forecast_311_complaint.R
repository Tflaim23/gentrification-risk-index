library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)
library(readxl)

data <- read_xlsx("data_clean/mac_calls_by_tract.xlsx") %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(call_volume), call_volume > 0)

ts_data <- data %>%
  mutate(log_calls = log(call_volume)) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
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
    model(ts_data, arima = ARIMA(log_calls ~ trend() + pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) NULL)
  
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
  tryCatch({
    model(ts_data, arima = ARIMA(log_calls ~ trend() + pdq(1, 1, 0)))
  }, error = function(e2) NULL)
}

models <- ts_data %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

forecast_data <- models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

forecast_clean <- forecast_data %>%
  filter(!is.na(.mean)) %>%
  mutate(call_volume = exp(.mean)) %>%
  select(GEOID, year, call_volume) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, call_volume)

write_csv(forecast_clean, "outputs/forecast_311_call_volume_by_tract.csv")
write_xlsx(forecast_clean, "outputs/forecast_311_call_volume_by_tract.xlsx")

model_orders <- models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
