library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

demographics_data <- read_csv("data_raw/percent_white_by_tract_raw.csv")
  select(GEOID, NAME, year, percent_white) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  filter(!is.na(percent_white), percent_white >= 0, percent_white <= 100) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%  
  ungroup() %>%
  distinct() %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts_data) {
  tryCatch({
    model(ts_data, arima = ARIMA(percent_white ~ pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) {
    message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
    tryCatch({
      model(ts_data, arima = ARIMA(percent_white ~ pdq(1, 1, 0)))
    }, error = function(e2) {
      message("Fallback also failed for GEOID: ", unique(ts_data$GEOID))
      return(NULL)
    })
  })
}

demo_models <- demo_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup()

demo_models_valid <- demo_models %>%
  filter(!map_lgl(model, is.null))

demo_forecast <- demo_models_valid %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

demo_forecast_clean <- demo_forecast %>%
  filter(!is.na(.mean)) %>%
  select(GEOID, year, .mean) %>%
  rename(percent_white = .mean) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, percent_white)

write_csv(demo_forecast_clean, "outputs/forecast_percent_white_by_tract_raw.csv")
write_xlsx(demo_forecast_clean, "outputs/forecast_percent_white_by_tract.xlsx")

model_orders <- demo_models_valid %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      arima_model <- mdl_tbl$arima[[1]]
      spec <- arima_model$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

order_summary <- model_orders %>%
  count(arima_order, sort = TRUE)

print(order_summary)
