intraday_model_interface <- function(lazy_tbl) {
  lazy_tbl |>
    filter(
      variable == "calories_out_intraday"
    ) |>
    select(time, value) |>
    arrange(time) |>
    collect() |>
    slice(-n())
}

#' @importFrom hms as_hms
#' @importFrom lubridate as_date wday
intraday_model <- function(d) {
  x <-
    d |>
    mutate(
      value = fitbit_calories_correction(value, scale = 1 / 96),
      weekday = wday(time, label = TRUE),
      hms = as_hms(time)
    )

  ## Create a profile of the average value for each time of each weekday
  week_profile <-
    x |> summarize(mean = mean(value), .by = c("hms", "weekday"))

  ## Calculate the deviance from the average for each time of each weekday
  profile_deviance <-
    x |>
    left_join(week_profile, by = c("hms", "weekday")) |>
    mutate(
      deviance = value - mean
    )

  ## Fit an ARIMA model to the deviance
  m <- forecast::auto.arima(
    profile_deviance$deviance, d = 0, D = 0,
    seasonal = FALSE,
    stationary = TRUE,
    stepwise = TRUE,
    approximation = TRUE,
    allowdrift = FALSE,
    allowmean = FALSE
  )

  ## Forecast the deviance for the next 24 hours
  fcst <-
    tibble(
      time = seq(max(profile_deviance$time) + minutes(15),
                 length.out = 96, by = "15 mins"),
      deviance = forecast::forecast(m, h = 96) |>
        pluck("mean") |>
        as.numeric()
    ) |>
    mutate(
      hms = as_hms(time),
      weekday = wday(time, label = TRUE),
      forecast = TRUE
    )

  average_day_calories <- sum(week_profile$mean) / 7

  combined <-
    profile_deviance |>
    ## tail(96) |>
    filter(time > max(profile_deviance$time) - days(1)) |>
    transmute(hms, weekday, time, deviance, forecast = FALSE) |>
    bind_rows(fcst) |>
    filter(as_date(time) == as_date(last(profile_deviance$time))) |>
    left_join(week_profile, by = c("hms", "weekday")) |>
    mutate(
      value = deviance + mean
    ) |>
    select(time, forecast, mean, value) |>
    pivot_longer(c(mean, value), names_to = "type", values_to = "value") |>
    transmute(
      time,
      variable = case_when(
        type == "mean" ~ "Typical day",
        forecast ~ "Forecast",
        TRUE ~ "Observation"
      ),
      value = value / average_day_calories
    )

  combined
}
