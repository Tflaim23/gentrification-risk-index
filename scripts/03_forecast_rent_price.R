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

zori_data <- read_csv("data_raw/zori_by_tract_year_clean.csv") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(zori_avg_weighted = ifelse(
    year > min(year) & zori_avg_weighted > 5 * lag(zori_avg_weighted),
    NA_real_,
    zori_avg_weighted
  )) %>%
  ungroup()

zori_ts <- zori_data %>%
  select(GEOID, year, zori_avg_weighted) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  filter(!is.na(zori_avg_weighted), zori_avg_weighted > 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  arrange(year) %>%
  mutate(
    lag_value = lag(zori_avg_weighted),
    ratio = zori_avg_weighted / lag_value,
    valid_ratio = is.na(ratio) | (ratio >= 0.2 & ratio <= 5)
  ) %>%
  filter(all(valid_ratio)) %>%
  mutate(
    pct_change = abs((zori_avg_weighted - lag_value) / lag_value),
    keep_row = ifelse(is.na(pct_change), TRUE, pct_change <= 0.5)
  ) %>%
  filter(keep_row) %>%
  mutate(
    log_zori = log(zori_avg_weighted)
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
    tryCatch({ model(ts_data, arima = ARIMA(formula)) }, error = function(e) NULL)
  }
  
  full_model <- try_model(log_zori ~ trend() + pdq(1:2, 1, 0:2))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(log_zori ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(log_zori ~ 1)
}

zori_models <- zori_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

valid_zori_models <- zori_models %>%
  mutate(order_check = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "unknown")
  })) %>%
  filter(order_check != "(0,0,0)", order_check != "unknown")

zori_forecast <- valid_zori_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast) %>%
  group_by(GEOID) %>%
  arrange(year) %>%
  mutate(
    smoothed_log = zoo::rollapply(.mean, width = 3, FUN = mean, fill = NA, align = "right"),
    smoothed_log = ifelse(is.na(smoothed_log), .mean, smoothed_log),
    zori_forecast = exp(smoothed_log)
  ) %>%
  ungroup() %>%
  mutate(NAME = NA_character_) %>%
  arrange(GEOID, year) %>%
  select(GEOID, NAME, year, zori_forecast)

write_csv(zori_forecast, "outputs/forecast_zori_by_tract.csv")
write_xlsx(zori_forecast, "outputs/forecast_zori_by_tract.xlsx")

model_orders <- valid_zori_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
