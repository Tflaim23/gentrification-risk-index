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
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  filter(!is.na(vacancy_rate), vacancy_rate > 0)

vacancy_ts <- vacancy_data %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)

fit_arima_safe <- function(ts_data) {
  tryCatch({
    model(ts_data, arima = ARIMA(vacancy_rate ~ pdq(p = 1:3, d = 1, q = 0:3)))
  }, error = function(e) {
    message("Fallback model used for GEOID: ", unique(ts_data$GEOID))
    tryCatch({
      model(ts_data, arima = ARIMA(vacancy_rate ~ pdq(1, 1, 0)))
    }, error = function(e2) {
      message("Fallback also failed for GEOID: ", unique(ts_data$GEOID))
      return(NULL)
    })
  })
}

vacancy_models <- vacancy_ts %>%
  group_by(GEOID) %>%
  nest() %>%
  mutate(model = map(data, fit_arima_safe)) %>%
  ungroup()

vacancy_models_valid <- vacancy_models %>%
  filter(!map_lgl(model, is.null))

vacancy_forecast <- vacancy_models_valid %>%
  mutate(forecast = map(model, forecast, h = 5)) %>%
  select(GEOID, forecast) %>%
  unnest(forecast)

vacancy_forecast_clean <- vacancy_forecast %>%
  filter(!is.na(.mean)) %>%
  select(GEOID, year, .mean) %>%
  rename(vacancy_rate_forecast = .mean) %>%
  mutate(
    vacancy_rate_forecast = pmax(0, pmin(vacancy_rate_forecast, 100)),  # Clamp to [0, 100]
    NAME = NA_character_
  ) %>%
  select(GEOID, NAME, year, vacancy_rate_forecast)

write_csv(vacancy_forecast_clean, "outputs/forecast_vacancy_rate_by_tract.csv")
write_xlsx(vacancy_forecast_clean, "outputs/forecast_vacancy_rate_by_tract.xlsx")

model_orders <- vacancy_models_valid %>%
  mutate(arima_order = map_chr(model, function(mdl_tbl) {
    tryCatch({
      arima_model <- mdl_tbl$arima[[1]]
      spec <- arima_model$fit$spec
      paste0("(", spec$p, ",", spec$d, ",", spec$q, ")")
    }, error = function(e) NA_character_)
  })) %>%
  select(GEOID, arima_order)

print(
  model_orders %>%
    count(arima_order, sort = TRUE)
)
