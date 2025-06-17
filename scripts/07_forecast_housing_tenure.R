library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(zoo)
library(writexl)

raw <- read_csv("data_raw/housing_tenure_burden_raw.csv") %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year),
    housing_pressure_score = renter_occupancy_rate * percent_rent_burdened / 100
  ) %>%
  filter(!is.na(housing_pressure_score), housing_pressure_score > 0) %>%
  arrange(GEOID, year)

clipped <- raw %>%
  group_by(GEOID) %>%
  mutate(
    lag_val = lag(housing_pressure_score),
    ratio = housing_pressure_score / lag_val,
    keep = is.na(ratio) | (ratio <= 5 & ratio >= 0.2)
  ) %>%
  filter(keep) %>%
  select(-lag_val, -ratio, -keep) %>%
  ungroup()

ts_data <- clipped %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  mutate(log_score = log(housing_pressure_score)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

validate_forecast <- function(values) {
  if (any(is.na(values)) || length(values) < 2) return(FALSE)
  max_growth <- max(values[-1] / values[-length(values)], na.rm = TRUE)
  return(max_growth <= 1.5)
}

fit_arima_safely <- function(ts) {
  try_model <- function(formula) {
    tryCatch({ model(ts, arima = ARIMA(formula)) }, error = function(e) NULL)
  }
  
  full_model <- try_model(log_score ~ trend() + pdq(1:3, 1, 0:3))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    predicted <- exp(fc$.mean)
    if (validate_forecast(predicted)) return(full_model)
  }
  
  fallback_model <- try_model(log_score ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(log_score ~ 1)
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
  unnest(forecast) %>%
  group_by(GEOID) %>%
  arrange(year) %>%
  mutate(
    forecast_raw = exp(.mean),
    smoothed = zoo::rollapply(forecast_raw, width = 3, FUN = mean, fill = NA, align = "right"),
    smoothed = ifelse(is.na(smoothed), forecast_raw, smoothed),
    housing_pressure_forecast = pmin(100, pmax(0, smoothed))
  ) %>%
  ungroup() %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, housing_pressure_forecast) %>%
  arrange(GEOID, year)

write_csv(forecast_data, "outputs/forecast_housing_pressure_by_tract.csv")
write_xlsx(forecast_data, "outputs/forecast_housing_pressure_by_tract.xlsx")

model_orders <- models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
