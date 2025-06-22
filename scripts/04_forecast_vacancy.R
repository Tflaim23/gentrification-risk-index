library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

vacancy_data <- read_csv("data_raw/vacancy_rate_by_tract_raw.csv") %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(vacancy_rate = ifelse(
    year > min(year) &
      (vacancy_rate > 5 * lag(vacancy_rate) | vacancy_rate < 0.2 * lag(vacancy_rate)),
    NA_real_,
    vacancy_rate
  )) %>%
  ungroup()

vacancy_ts <- vacancy_data %>%
  filter(!is.na(vacancy_rate), vacancy_rate >= 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  arrange(year) %>%
  mutate(
    lag_val = lag(vacancy_rate),
    valid_ratio = is.na(lag_val) | (vacancy_rate / lag_val >= 0.2 & vacancy_rate / lag_val <= 5),
    pct_delta = abs((vacancy_rate - lag_val) / lag_val),
    keep_row = is.na(pct_delta) | pct_delta <= 0.5
  ) %>%
  filter(all(valid_ratio), keep_row) %>%
  ungroup() %>%
  distinct() %>%
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
  
  full_model <- try_model(vacancy_rate ~ trend() + pdq(1:3, 1, 0:3))
  if (!is.null(full_model)) {
    fc <- forecast(full_model, h = 5)
    if (validate_forecast(fc$.mean)) return(full_model)
  }
  
  fallback_model <- try_model(vacancy_rate ~ trend())
  if (!is.null(fallback_model)) return(fallback_model)
  
  try_model(vacancy_rate ~ 1)
}

vacancy_models <- vacancy_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safely)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

valid_models <- vacancy_models %>%
  mutate(order_check = map_chr(model, function(m) {
    tryCatch({
      spec <- m$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "unknown")
  })) %>%
  filter(order_check != "(0,0,0)", order_check != "unknown")

vacancy_forecast <- valid_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast) %>%
  group_by(GEOID) %>%
  arrange(year) %>%
  ungroup() %>%
  group_split(GEOID) %>%
  map_df(function(df) {
    if (n_distinct(df$year) >= 3) {
      smoothed <- tryCatch({
        suppressWarnings(predict(loess(.mean ~ year, span = 0.75), newdata = data.frame(year = df$year)))
      }, error = function(e) df$.mean)
    } else {
      smoothed <- df$.mean
    }
    
    df$vacancy_rate_forecast <- pmin(100, pmax(0, smoothed))
    df$NAME <- NA_character_
    df
  }) %>%
  select(GEOID, NAME, year, vacancy_rate_forecast)


write_csv(vacancy_forecast, "outputs/forecast_vacancy_rate_by_tract.csv")
write_xlsx(vacancy_forecast, "outputs/forecast_vacancy_rate_by_tract.xlsx")

model_orders <- valid_models %>%
  mutate(arima_order = map_chr(model, function(m) {
    tryCatch({
      spec <- m$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) "trend")
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
