library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

zori_data <- read_csv("data_raw/zori_by_tract_year_clean.csv")

zori_ts <- zori_data %>%
  select(GEOID, year, zori_avg_weighted) %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(zori_avg_weighted), zori_avg_weighted > 0) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  mutate(log_zori = log(zori_avg_weighted)) %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts_data) {
  tryCatch({
    model(ts_data, arima = ARIMA(log_zori ~ pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) {
    message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
    tryCatch({
      model(ts_data, arima = ARIMA(log_zori ~ pdq(1, 1, 0)))
    }, error = function(e2) {
      return(NULL)
    })
  })
}

zori_models <- zori_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

zori_forecast <- zori_models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

zori_forecast_clean <- zori_forecast %>%
  filter(!is.na(.mean)) %>%
  mutate(zori_forecast = exp(.mean)) %>%
  select(GEOID, year, zori_forecast) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, zori_forecast)

write_csv(zori_forecast_clean, "outputs/forecast_zori_by_tract.csv")
write_xlsx(zori_forecast_clean, "outputs/forecast_zori_by_tract.xlsx")

model_orders <- zori_models %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      spec <- mdl_tbl$arima[[1]]$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(model_orders %>% count(arima_order, sort = TRUE))
