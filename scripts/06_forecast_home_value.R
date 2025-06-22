library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

zhvi_data <- read_csv("data_raw/zhvi_by_tract_year_clean.csv") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(zhvi_avg_weighted = ifelse(
    year > min(year) &
      (zhvi_avg_weighted > 5 * lag(zhvi_avg_weighted) |
         zhvi_avg_weighted < 0.2 * lag(zhvi_avg_weighted)),
    NA_real_,
    zhvi_avg_weighted
  )) %>%
  ungroup()

zhvi_ts <- zhvi_data %>%
  select(GEOID, year, zhvi_avg_weighted) %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(zhvi_avg_weighted), zhvi_avg_weighted > 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  arrange(year) %>%
  mutate(
    lag_value = lag(zhvi_avg_weighted),
    ratio = zhvi_avg_weighted / lag_value,
    valid_ratio = is.na(ratio) | (ratio >= 0.2 & ratio <= 5),
    pct_change = abs((zhvi_avg_weighted - lag_value) / lag_value),
    keep_row = ifelse(is.na(pct_change), TRUE, pct_change <= 0.5)
  ) %>%
  filter(all(valid_ratio), keep_row) %>%
  mutate(log_zhvi = log(zhvi_avg_weighted)) %>%
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
  
  full_model <- try_model(log_zhvi ~ trend() + pdq(1:3, 1, 0:3))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(log_zhvi ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(log_zhvi ~ 1)
}

zhvi_models <- zhvi_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

valid_zhvi_models <- zhvi_models %>%
  mutate(order_check = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "unknown")
  })) %>%
  filter(order_check != "(0,0,0)", order_check != "unknown")

zhvi_forecast <- valid_zhvi_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast) %>%
  group_split(GEOID) %>%
  map_df(function(df) {
    if (nrow(df) >= 3) {
      smoothed_log <- tryCatch({
        predict(loess(.mean ~ year, data = df, span = 0.75), newdata = df$year)
      }, error = function(e) df$.mean)
    } else {
      smoothed_log <- df$.mean
    }
    df %>%
      mutate(
        smoothed_log = smoothed_log,
        zhvi_forecast = exp(smoothed_log),
        NAME = NA_character_
      )
  }) %>%
  arrange(GEOID, year) %>%
  select(GEOID, NAME, year, zhvi_forecast)

write_csv(zhvi_forecast, "outputs/forecast_zhvi_by_tract.csv")
write_xlsx(zhvi_forecast, "outputs/forecast_zhvi_by_tract.xlsx")

model_orders <- valid_zhvi_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
