library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(writexl)
library(tidyr)
library(feasts)
library(purrr)
library(fabletools)
library(zoo)

income_data <- read_csv("data_raw/income_by_tract_raw.csv")

income_data <- read_csv("data_raw/income_by_tract_raw.csv")

income_data <- income_data %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(estimate = ifelse(
    year > min(year) & estimate > 5 * lag(estimate),
    NA,
    estimate
  )) %>%
  ungroup()

income_ts <- income_data %>%
  filter(!is.na(estimate), !is.na(moe)) %>%
  filter(moe / estimate <= 0.5, estimate > 0) %>%
  select(GEOID, NAME, year, estimate) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  arrange(year) %>%
  mutate(
    lag_estimate = lag(estimate),
    ratio = estimate / lag_estimate,
    valid_ratio = is.na(ratio) | (ratio >= 0.2 & ratio <= 5)
  ) %>%
  filter(all(valid_ratio)) %>%
  mutate(
    pct_change = abs((estimate - lag_estimate) / lag_estimate),
    keep_row = ifelse(is.na(pct_change), TRUE, pct_change <= 0.5)
  ) %>%
  filter(keep_row) %>%
  mutate(
    log_estimate = log(estimate)
  ) %>%
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
  try_model <- function(formula) {
    tryCatch({
      model(ts_data, arima = ARIMA(formula))
    }, error = function(e) NULL)
  }
  
  full_model <- try_model(log_estimate ~ trend() + pdq(1:2, 1, 0:2))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(log_estimate ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(log_estimate ~ 1)
}

income_models <- income_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

valid_income_models <- income_models %>%
  mutate(order_check = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "unknown")
  })) %>%
  filter(order_check != "(0,0,0)", order_check != "unknown")

income_forecast <- valid_income_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast) %>%
  group_by(GEOID) %>%
  arrange(year) %>%
  mutate(
    smoothed_log = zoo::rollapply(.mean, width = 3, FUN = mean, fill = NA, align = "right"),
    smoothed_log = ifelse(is.na(smoothed_log), .mean, smoothed_log),
    estimate = exp(smoothed_log)
  ) %>%
  ungroup() %>%
  mutate(NAME = NA_character_) %>%
  arrange(GEOID, year) %>%
  select(GEOID, NAME, year, estimate)

write_csv(income_forecast, "outputs/forecast_income_by_tract.csv")
write_xlsx(income_forecast, "outputs/forecast_income_by_tract.xlsx")

model_orders <- valid_income_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
