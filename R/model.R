input_data <- function() {
  readRDS("data.rds") |>
    filter(
      ## Outliers
      value < 900 & variable == "calories_in"
    ) |>
    summarize(
      ## Average multiple measurements
      value = mean(value, na.rm = TRUE),
      .by = c("variable", "time")
    )
}

fitbit_calories_correction <- function(x, scale = 1) {
  (x - 1500*scale) * 0.41 + 1500*scale
}

#' @importFrom zoo zoo
#' @import KFAS
#' @export
model <- function(d = input_data(), n = 0L) {
  d_wide <-
    d |>
    pivot_wider(names_from = "variable") |>
    mutate(
      calories_out = fitbit_calories_correction(calories_out)
    )
  ## Add NA values to the end of the dataset predict states
  if (n > 0L) {
    d_wide <-
      d_wide |> full_join(
        tibble(time = seq(max(d_wide$time) + 1L, length.out = 30, by = "days")),
        by = "time"
      )
  }
  observation_names <- c("weight", "calories_in", "calories_out")
  mts <- zoo(d_wide[observation_names], d_wide[["time"]])

  weight_obs_se <- 0.1
  calories_in_obs_se <- 10
  calories_out_obs_se <- NA
  observation_equation <- matrix(c(
    1, 0, 0, 0, 0, 0, 1,
    0, 1, 1, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 0
  ), nrow = length(observation_names), byrow = TRUE)
  observation_variance <-
    diag(c(weight_obs_se, calories_in_obs_se, calories_out_obs_se)^2)

  alpha <- 1 / 7700
  retention_alpha <- 0.4

  state_names <- c(
    "weight", "calories_in", "calories_in_mean",
    "calories_out", "calories_out_mean", "calories_bias", "retention"
  )
  state_equation <- matrix(c(
    1, alpha, alpha, -alpha, -alpha, 0, 0, # weight
    0, 0.7, 0, 0, 0, 0, 0, # calories_in
    0, 0, 1, 0, 0, 0, 0, # calories_in_mean
    0, 0, 0, 0.7, 0, 0, 0, # calories_out
    0, 0, 0, 0, 1, 0, 0, # calories_out_mean
    0, 0, 0, 0, 0, 1, 0, # calories_bias
    0, 0, 0, 0, 0, 0, retention_alpha # retention
  ), nrow = length(state_names), byrow = TRUE)

  se <- c(
    weight_eq = 0.0001,
    calories_in = 500,
    calories_in_mean = 100,
    calories_out = 500,
    calories_out_mean = 100,
    calories_bias = 5,
    retention = NA
  )
  state_variance <- diag(se^2)

  weight_init <-
    d |>
    filter(variable == "weight") |>
    filter(time == min(time)) |>
    pull(value)
  init <- tribble(
    ~variable, ~value, ~variance,
    "weight", weight_init, 100,
    "calories_in", 0, 10,
    "calories_in_mean", 2300, 1000^2,
    "calories_out", 0, 10,
    "calories_out_mean", 2300, 1000^2,
    "calories_bias", 0, 1000^2,
    "retention", 0, 1
  )
  init_state <- init$value
  init_variance <- diag(init$variance)

  # Define the state-space model
  mod <- SSModel(
    mts ~ -1 + SSMcustom(
      Z = observation_equation,
      T = state_equation,
      R = diag(dim(state_equation)[1]),
      Q = state_variance,
      a1 = init_state,
      P1 = init_variance,
      state_names = state_names
    ),
    H = observation_variance
  )

  # Fit the model using maximum likelihood
  fit <- fitSSM(mod, inits = rep(1, 4), method = "BFGS")
  #print(fit$optim.out)

  # Estimate the states
  kf <- KFS(fit$model, filtering = "state", smoothing = "state")
  kf
}
#' @export
output_data <- function(d, kf) {
  states <-
    as_tibble(kf$alphahat) |>
    mutate(time = d$time)
  combined <-
    bind_rows(
      observation = d,
      state = states,
      .id = "type"
    ) |>
    pivot_longer(-c(type, time), names_to = "variable")
}

#' @importFrom stats predict
forecast <- function(kf, n = 100) {
  time <- seq(max(attr(kf$model$y, "index")) + 1L, length.out = n, by = "days")
  predict(kf$model, n.ahead = n) |>
    map(as.double) |>
    as_tibble() |>
    mutate(time = time) |>
    pivot_longer(-time, names_to = "variable")
}

#' @export
combined_data <- function(d, kf = model(d), n = 30,
                          kf_state = model(d, n)) {
  states <-
    as_tibble(kf_state$alphahat) |>
    mutate(
      time = attr(kf_state$model$y, "index"),
      calories_in = calories_in + calories_in_mean,
      calories_out = calories_out + calories_out_mean
    ) |>
    ## select(-calories_in_mean) |>
    pivot_longer(-time, names_to = "variable")
  ## TODO: Is observation forecast necessary?
  f <- forecast(kf, n)
  bind_rows(
    observation = d,
    state = states,
    forecast = f,
    .id = "type"
  )
}
