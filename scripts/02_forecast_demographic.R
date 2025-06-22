library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(writexl)
library(tidyr)
library(feasts)
library(purrr)
library(fabletools)

demographics_data <- read_csv("data_raw/percent_white_by_tract_raw.csv") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(percent_white = ifelse(
    year > min(year) & percent_white > 5 * lag(percent_white),
    NA_real_,
    percent_white
  )) %>%
  ungroup()

white_ts <- demographics_data %>%
  select(GEOID, NAME, year, percent_white) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  filter(!is.na(percent_white), percent_white >= 0, percent_white <= 100) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  arrange(year) %>%
  mutate(
    lag_value = lag(percent_white),
    ratio = percent_white / lag_value,
    valid_ratio = is.na(ratio) | (ratio >= 0.2 & ratio <= 5),
    pct_change = abs((percent_white - lag_value) / lag_value),
    keep_row = ifelse(is.na(pct_change), TRUE, pct_change <= 0.5)
  ) %>%
  filter(all(valid_ratio), keep_row) %>%
  ungroup() %>%
  distinct() %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

validate_forecast <- function(values) {
  if (any(is.na(values)) || length(values) < 2) return(FALSE)
  max_growth <- max(values[-1] / values[-length(values)], na.rm = TRUE)
  max_growth <= 1.5
}

fit_arima_safely <- function(ts_data) {
  try_model <- function(formula) {
    tryCatch({
      model(ts_data, arima = ARIMA(formula))
    }, error = function(e) NULL)
  }
  
  full_model <- try_model(percent_white ~ trend() + pdq(1:2, 1, 0:2))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(percent_white ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(percent_white ~ 1)
}

white_models <- white_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

valid_white_models <- white_models %>%
  mutate(order_check = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "unknown")
  })) %>%
  filter(order_check != "(0,0,0)", order_check != "unknown")

white_forecast <- valid_white_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast) %>%
  group_split(GEOID) %>%
  map_df(function(df) {
    if (nrow(df) >= 3) {
      smoothed <- tryCatch({
        predict(loess(.mean ~ year, data = df, span = 0.75), newdata = df$year)
      }, error = function(e) df$.mean)
    } else {
      smoothed <- df$.mean
    }
    df %>%
      mutate(
        smoothed = smoothed,
        percent_white = pmin(100, pmax(0, smoothed)),
        NAME = NA_character_
      )
  }) %>%
  arrange(GEOID, year) %>%
  select(GEOID, NAME, year, percent_white)

write_csv(white_forecast, "outputs/forecast_percent_white_by_tract.csv")
write_xlsx(white_forecast, "outputs/forecast_percent_white_by_tract.xlsx")

model_orders <- valid_white_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
