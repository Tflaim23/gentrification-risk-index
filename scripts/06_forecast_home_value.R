library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

zhvi_data <- read_csv("data_raw/zhvi_by_tract_year_clean.csv")

zhvi_ts <- zhvi_data %>%
  select(GEOID, year, zhvi_avg_weighted) %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(zhvi_avg_weighted), zhvi_avg_weighted > 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  mutate(log_zhvi = log(zhvi_avg_weighted)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts_data) {
  tryCatch({
    model(ts_data, arima = ARIMA(log_zhvi ~ pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) {
    message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
    tryCatch({
      model(ts_data, arima = ARIMA(log_zhvi ~ pdq(1, 1, 0)))
    }, error = function(e2) {
      return(NULL)
    })
  })
}

zhvi_models <- zhvi_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

zhvi_forecast <- zhvi_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

zhvi_forecast_clean <- zhvi_forecast %>%
  filter(!is.na(.mean)) %>%
  mutate(zhvi_forecast = exp(.mean)) %>%
  select(GEOID, year, zhvi_forecast) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, zhvi_forecast)

write_csv(zhvi_forecast_clean, "outputs/forecast_zhvi_by_tract.csv")
write_xlsx(zhvi_forecast_clean, "outputs/forecast_zhvi_by_tract.xlsx")

model_orders <- zhvi_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
