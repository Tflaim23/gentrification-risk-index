library(dplyr)
library(readr)
library(tsibble)
library(fable)
library(writexl)
library(tidyr)
library(feasts)
library(purrr)
library(fabletools)

income_data <- read_csv("data_raw/income_by_tract_raw.csv")

income_ts <- income_data %>%
  filter(!is.na(estimate), !is.na(moe)) %>%
  filter(moe / estimate <= 0.5) %>% 
  select(GEOID, NAME, year, estimate) %>%
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  ) %>%
  group_by(GEOID) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  distinct() %>%
  as_tsibble(index = year, key = GEOID) %>%
  fill_gaps(.full = TRUE)


income_models <- income_ts %>%
  model(arima = ARIMA(estimate ~ pdq(p = 1:3, d = 1, q = 0:3) + pdq(p = 0:3, d = 1, q = 1:3)))

income_forecast <- income_models %>%
  forecast(h = 5)

income_forecast_clean <- income_forecast %>%
  as_tibble() %>%
  filter(!is.na(.mean)) %>%
  select(GEOID, year, .mean) %>%
  rename(estimate = .mean) %>%
  mutate(NAME = NA_character_) %>%
  select(GEOID, NAME, year, estimate)

write_csv(income_forecast_clean, "outputs/forecast_income_by_tract_raw.csv")
write_xlsx(income_forecast_clean, "outputs/forecast_income_by_tract.xlsx")

model_orders <- income_models %>%
  mutate(
    arima_order = map_chr(arima, function(m) {
      tryCatch({
        p <- m$fit$spec$p
        d <- m$fit$spec$d
        q <- m$fit$spec$q
        paste0("(", p, ",", d, ",", q, ")")
      }, error = function(e) NA_character_)
    })
  ) %>%
  as_tibble() %>%
  select(GEOID, arima_order)

print(
  model_orders %>%
    count(arima_order) %>%
    arrange(desc(n))
)
