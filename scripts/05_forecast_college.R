library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tidyr)
library(purrr)
library(writexl)

data <- read_csv("data_raw/education_by_tract_raw.csv") %>%
  mutate(GEOID = as.character(GEOID), year = as.integer(year)) %>%
  filter(!is.na(percent_bachelors_plus), percent_bachelors_plus >= 0)

ts_data <- data %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts) {
  tryCatch(
    model(ts, arima = ARIMA(percent_bachelors_plus ~ pdq(p = 1:3, d = 1, q = 0:3))),
    error = function(e) {
      message("Fallback for GEOID: ", unique(ts$GEOID))
      tryCatch(
        model(ts, arima = ARIMA(percent_bachelors_plus ~ pdq(1, 1, 0))),
        error = function(e2) NULL
      )
    }
  )
}

models <- ts_data %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup() %>%
  filter(!map_lgl(model, is.null))

forecast_data <- models %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

forecast_clean <- forecast_data %>%
  filter(!is.na(.mean)) %>%
  transmute(
    GEOID,
    NAME = NA_character_,
    year,
    percent_bachelors_plus = pmin(100, pmax(0, .mean))
  )

write_csv(forecast_clean, "outputs/forecast_education_by_tract.csv")
write_xlsx(forecast_clean, "outputs/forecast_education_by_tract.xlsx")

model_orders <- models %>%
  mutate(arima_order = map_chr(model, ~tryCatch({
    spec <- .$arima[[1]]$fit$spec
    paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
  }, error = function(e) NA_character_))) %>%
  count(arima_order, sort = TRUE)

print(model_orders)