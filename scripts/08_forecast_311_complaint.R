library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(readxl)
library(writexl)
library(zoo)

raw <- read_xlsx("data_clean/mac_calls_by_tract.xlsx") %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(call_volume), call_volume > 0) %>%
  arrange(GEOID, year)

clipped <- raw %>%
  group_by(GEOID) %>%
  mutate(
    lag_val = lag(call_volume),
    ratio = call_volume / lag_val,
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
  mutate(log_calls = log(call_volume)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

validate_forecast <- function(log_vals) {
  vals <- exp(log_vals)
  if (any(is.na(vals)) || length(vals) < 2) return(FALSE)
  max_growth <- max(vals[-1] / vals[-length(vals)], na.rm = TRUE)
  return(max_growth <= 1.5)
}

fit_arima_safely <- function(ts) {
  try_model <- function(formula) {
    tryCatch(model(ts, arima = ARIMA(formula)), error = function(e) NULL)
  }
  
  full_model <- try_model(log_calls ~ trend() + pdq(1:3, 1, 0:3))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(log_calls ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(log_calls ~ 1)
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
    call_volume = pmax(0, smoothed)  
  ) %>%
  ungroup() %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, call_volume) %>%
  arrange(GEOID, year)

write_csv(forecast_data, "outputs/forecast_311_call_volume_by_tract.csv")
write_xlsx(forecast_data, "outputs/forecast_311_call_volume_by_tract.xlsx")

model_orders <- models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
