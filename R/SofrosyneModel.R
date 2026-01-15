sofrosyne_model_interface <- function(lazy_tbl) {
  lazy_tbl |>
    select(time, variable, value) |>
    filter(
      variable %in% c("weight", "fat") |
        (variable %in% c("calories_in", "calories_out") &
           time < today())
    ) |>
    filter(
      ## Outliers
      !(value < 900 & variable == "calories_in")
    ) |>
    mutate(
      time = as.Date(time)
    ) |>
    summarise(
      value = mean(value, na.rm = TRUE),
      .by = c("time", "variable")
    ) |>
    arrange(time) |>
    collect()
}

#' Compute basal metabolic rate
bmr <- function(weight, time, height, birth_date, sex) {
  ## The Mifflin St Jeor equation
  ## https://en.wikipedia.org/wiki/Basal_metabolic_rate
  age <- as.numeric(difftime(time, birth_date, units = "days"))
  sex_factor <- switch(sex, male = 5, female = -161, other = 0)
  10 * weight + 6.25 * height - (5 / 365.2425) * age + sex_factor
}

#' @export
SofrosyneModel <- R6Class(
  "SofrosyneModel",
  portable = FALSE,
  public = list(
    params = NULL,
    raw_data = NULL,
    input_data = NULL,
    peak_weight = NULL,
    peak_bmr = NULL,
    model_object = NULL,
    fitted_model = NULL,
    states = NULL,
    combined_data = NULL,
    forecast_horizon = 30,
    default_params = list(
      person = list(
        birth_date = as.Date("1983-05-03"),
        height = 183,
        sex = "male",
        activity_measurement_correction_factor = 1/0.4
      ),
      energy_matrix = list(
        e_to_fat = 8938,
        e_to_lean = 29793,
        comp_to_fat = -0.383,
        comp_to_lean = 0.8850
      ),
      retention_alpha = 0.4,
      obs_se = list(
        weight = 0.01151251, # NA,
        fat = 0.5,
        calories_in = 10,
        calories_out = 100
      ),
      state_se = list(
        fat = 0.001,
        lean = 0.0001,
        calories_in_residual = 1000,
        calories_in_mean = 100,
        calories_out_residual = 500,
        calories_out_mean = 50,
        adjusted_bmr = 0,
        calories_bias = 3,
        composition = 0.005,
        retention = 0.5954865,
        constant = 0,
        t = 0
      ),
      init_state = list(
        fat = NULL,
        lean = NULL,
        calories_in_residual = 0,
        calories_in_mean = 2300,
        calories_out_residual = 0,
        calories_out_mean = 500,
        adjusted_bmr = NA,
        calories_bias = 0,
        composition = 0,
        retention = 0,
        constant = 1,
        t = NA
      ),
      init_se = list(
        fat = 10,
        lean = 10,
        calories_in_residual = 3,
        calories_in_mean = 1000,
        calories_out_residual = 3,
        calories_out_mean = 1000,
        adjusted_bmr = 0,
        calories_bias = 1000,
        composition = 0.05,
        retention = 1,
        constant = 0,
        t = 0
      )
    ),

    initialize = function(data, params = list(), forecast_horizon = 30L) {
      ## Validate data
      if (nrow(data) == 0) {
        stop("Data is empty")
      }
      if (!all(colnames(data) %in% c("time", "variable", "value"))) {
        stop("Data must contain columns: time, variable, value")
      }
      raw_data <<- data
      forecast_horizon <<- forecast_horizon
      params$init_state$t <- as.integer(min(raw_data$time, na.rm = TRUE))
      params <<- modifyList(default_params, params)
      prepare_input_data()
    },

    prepare_input_data = function() {
      if (!is.null(input_data)) {
        return(input_data)
      }
      d_wide <-
        self$raw_data |>
        pivot_wider(names_from = "variable") |>
        mutate(
          fat = (fat / 100) * weight
        )

      ## Remove fat outliers
      outliers <- abs(scale(resid(
        lm(fat ~ weight, data = d_wide, na.action = "na.omit")
      ))) > 3.5
      d_wide$fat[as.integer(names(outliers[outliers,]))] <- NA_real_

      ## Forecast horizon is set by adding NA values to the end of the dataset
      if (forecast_horizon > 0L) {
        forecast_dates <- seq(
          max(d_wide$time) + 1L,
          length.out = forecast_horizon,
          by = "days"
        )
        d_wide <-
          full_join(d_wide, tibble(time = forecast_dates), by = "time")
      }
      observation_names <- names(params$obs_se)
      input_data <<- zoo(d_wide[observation_names], d_wide[["time"]])
      peak_weight <<-
        params$person$peak_weight %||% max(input_data$weight, na.rm = TRUE)
      peak_bmr <<- bmr(input_data$weight, zoo::index(input_data),
                       params$person$height, params$person$birth_date,
                       params$person$sex) |>
        max(na.rm = TRUE)
    },

    ## SOFROSYNE MODEL SPECIFIC FUNCTIONS
    create_model_object = function(input_data = self$input_data) {
      if (!is.null(model_object)) {
        return(model_object)
      }

      state_names <- names(params$state_se)
      observation_names <- names(params$obs_se)

      observation_variance <- diag(unlist(params$obs_se)^2)

      ## observation_equation <- matrix(c(
      ##   1, 1, 0, 0, 0, 0, 0, 0, 1, # weight
      ##   1, 0, 0, 0, 0, 0, 0, 0, 0, # fat
      ##   0, 0, 1, 1, 0, 0, 0, 0, 0, # calories_in
      ##   0, 0, 0, 0, 1, 1, 1, 0, 0  # calories_out
      ## ), nrow = nrow(observation_variance), byrow = TRUE)

      convert_to_model_matrix <- function(def) {
        tbl <- map(def, function(obs) {
          replace(
            rep(0, length(state_names)),
            match(names(obs), state_names),
            obs
          )
        }) |>
          bind_rows()
        m <- t(as.matrix(tbl))
        colnames(m) <- state_names
        m
      }
      amcf <- params$person$activity_measurement_correction_factor
      observation_equation <- list(
        weight = c(lean = 1, fat = 1, retention = 1),
        fat    = c(fat = 1),
        calories_in  = c(calories_in_residual = 1, calories_in_mean = 1),
        calories_out = c(
          calories_out_residual = amcf, calories_out_mean = amcf,
          adjusted_bmr = 1, calories_bias = 1
        )
      ) |>
        convert_to_model_matrix()


      a <- 1 / params$energy_matrix$e_to_fat
      b <- 1 / params$energy_matrix$e_to_lean
      c <- params$energy_matrix$comp_to_fat
      d <- params$energy_matrix$comp_to_lean
      retention_alpha <- params$retention_alpha

      state_variance <- diag(unlist(params$state_se)^2)

      ## state_equation <- matrix(c(
      ##   1,  0,  a,    a,  -a,   -a,  0,    c,  0,  #  fat
      ##   0,  1,  b,    b,  -b,   -b,  0,    d,  0,  #  lean
      ##   0,  0,  0.7,  0,  0,    0,   0,    0,  0,  #  calories_in
      ##   0,  0,  0,    1,  0,    0,   0,    0,  0,  #  calories_in_mean
      ##   0,  0,  0,    0,  0.7,  0,   0,    0,  0,  #  calories_out
      ##   0,  0,  0,    0,  0,    1,   0,    0,  0,  #  calories_out_mean
      ##   0,  0,  0,    0,  0,    0,   1,    0,  0,  #  calories_bias
      ##   0,  0,  0,    0,  0,    0,   0,     0.99,  0, #  composition
      ##   0,  0,  0,    0,  0,    0,   0,  0, retention_alpha # retention
      ## ), nrow = length(state_names), byrow = TRUE)

      sex_factor <- switch(params$person$sex,
        male = 5,
        female = -161,
        other = 0
      )

      adjusted_bmr_eq <- c(
        lean = 10 + (peak_bmr / peak_weight),
        fat = 10 + (peak_bmr / peak_weight),
        t = - 5 / 365.2425,
        constant =
          (6.25 * params$person$height + sex_factor) -
          peak_bmr +
          (5 / 365.2425) * as.integer(params$person$birth_date)
      )

      state_equation <- list(
        fat = c(fat = 1, calories_in_residual = a, calories_in_mean = a,
                calories_out_residual = -a, calories_out_mean = -a, adjusted_bmr = -a,
                composition = c),
        lean = c(lean = 1, calories_in_residual = b, calories_in_mean = b,
                 calories_out_residual = -b, calories_out_mean = -b, adjusted_bmr = -b,
                 composition = d),
        calories_in_residual = c(calories_in_residual = 0.7),
        calories_in_mean = c(calories_in_mean = 1),
        calories_out_residual = c(calories_out_residual = 0.7),
        calories_out_mean = c(calories_out_mean = 1),
        adjusted_bmr = adjusted_bmr_eq,
        calories_bias = c(calories_bias = 1),
        composition = c(composition = 0.99),
        retention = c(retention = retention_alpha),
        constant = c(constant = 1),
        t = c(t = 1, constant = 1)
      ) |>
        convert_to_model_matrix()

      init_state <- params$init_state
      if (is.null(init_state$fat)) {
        init_state$fat <- Find(Negate(is.na), input_data$fat)
      }
      if (is.null(init_state$lean)) {
        init_state$lean <-
          Find(Negate(is.na), input_data$weight) - init_state$fat
      }

      ## browser()
      init_state$adjusted_bmr <-
        bmr(init_state$fat + init_state$lean,
            min(zoo::index(input_data), na.rm = TRUE),
            params$person$height, params$person$birth_date,
            params$person$sex) +
        peak_bmr * ((init_state$fat + init_state$lean) / peak_weight - 1)

      init_variance <- diag(unlist(params$init_se)^2)

      model_object <<- SSModel(
        input_data ~ -1 + SSMcustom(
          Z = observation_equation,
          T = state_equation,
          R = diag(dim(state_equation)[1]),
          Q = state_variance,
          a1 = unlist(init_state),
          P1 = init_variance,
          state_names = state_names
        ),
        H = observation_variance
      )
      model_object
    },
    fit_model = function(mod = create_model_object()) {
      if (is.null(fitted_model)) {
        fitted_model <<- fitSSM(mod, inits = rep(1, 4), method = "BFGS")
        ## fitted_model <<- fitSSM(
        ##   mod, inits = rep(1, 4), method = "BFGS"## ,
        ##   ## updatefn = function(pars, model, ...) {
        ##   ##   browser()
        ##   ##   model
        ##   ## }
        ## )
      }
      fitted_model
    },
    estimate_states = function(fit = fit_model()) {
      if (is.null(states)) {
        states <<- KFS(fit$model, filtering = "state", smoothing = "state")
      }
      states
    },
    logistic_compressor = function(x, scale = 100, cutpoint = 200, lowpoint = -200) {
      ## TODO: Does not work with different scales
      i_compress <- x < cutpoint
      factor <- 2*plogis(x[i_compress], cutpoint, 2*scale)
      y <- x
      y[i_compress] <- (factor*(cutpoint - lowpoint) + lowpoint)
      y
    },
    combine_data = function(kf = estimate_states()) {
      if (!is.null(combined_data)) {
        return(combined_data)
      }
      states <-
        as_tibble(kf$alphahat) |>
        mutate(
          time = attr(kf$model$y, "index"),
          weight = fat + lean,
          fat = 100 * fat / weight,
          calories_in = calories_in_residual + calories_in_mean,
          bmr = bmr(weight, time, params$person$height,
                    params$person$birth_date, params$person$sex),
          calories_out =
            adjusted_bmr + calories_out_residual + calories_out_mean,
          calories_out_fitbit =
            adjusted_bmr +
            params$person$activity_measurement_correction_factor *
            (calories_out_residual + calories_out_mean) +
            calories_bias,
          calories_out_mean = calories_out_mean + adjusted_bmr
        ) |>
        pivot_longer(-time, names_to = "variable")
      combined_data <<-
        bind_rows(
          observation = raw_data,
          state = states,
          .id = "type"
        )
      combined_data
    }
  )
)

SofrosyneModelAnalytics <- R6Class(
  "SofrosyneModelAnalytics",
  inherit = SofrosyneModel,
  portable = FALSE,
  public = list(
    clear_cache = function() {
      self$input_data <- NULL
      self$model_object <- NULL
      self$fitted_model <- NULL
      self$states <- NULL
      self$combined_data <- NULL
    },
    poos = function(samples = 150) {
      full_data <- raw_data
      on.exit(raw_data <<- full_data)
      tibble(
        cutoff = seq(max(d$time) - samples, max(d$time), by = 1) - 1
      ) |>
        mutate(
          data = map(cutoff, ~ {
            data |>
              filter(case_match(
                variable,
                "weight" ~ time <= .x,
                "calories_in" ~ time <= .x - 1,
                "calories_out" ~ time <= .x - 1
              ))
          }) |>
            map(~ {
              clear_cache()
              raw_data <<- .x
              combine_data()
            })
        )|>
        unnest(data)
    },
    state_deviance = function(variables = "weight") {
      poos() |>
        filter(type == "state", variable %in% variables) |>
        filter(time >= "2024-05-01") |>
        mutate(
          .by = c("time", "variable"),
          deviance = value - value[cutoff == max(cutoff)]
        )
    }
  )
)
