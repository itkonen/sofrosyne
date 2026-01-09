#' Fitbit Web API endpoints
#'
#' @importFrom R6 R6Class
#' @param collection_path The collection path. One of "activities", "body", "foods", "sleep", "userRevokedAccess", "all".
#' @param subscription_id The subscription id. Max length 50 characters.
FitbitWebAPI <- R6Class(
  "FitbitWebAPI",
  portable = FALSE,
  public = list(
    #' @field token A Fitbit user token
    token = NULL,
    #' @field rate_limit_key The rate limit key unique to the user
    rate_limit_key = NULL,
    #' @description Initialize the FitbitWebAPI object
    #' @param token A Fitbit user token
    initialize = function(token) {
      token <<- token
      rate_limit_key <<- paste0("fitbit-rl-", tolower(token$user_id))
    },
    #' @description Query the Fitbit Web API
    #' @param ep character vector of the endpoint
    #' @param method The HTTP method
    query = function(ep, method = "GET", error_message = NULL) {
      self$limit_rate()
      ep[length(ep)] <- paste0(ep[length(ep)], ".json")
      req <-
        request("https://api.fitbit.com/1/user") |>
        req_method(method) |>
        req_url_path_append(self$token$user_id, ep) |>
        req_auth_bearer_token(self$token$access_token) |>
        req_retry(3) |>
        req_fitbit_error(error_message)
      resp <-
        req_perform(req) |>
        resp_store_rate_limit()
      if (method != "DELETE") resp_body_json(resp)
    },
    #' @description Store the rate limit information
    #' Currently using a app wide rate limiter cache
    #' @param resp The response object
    resp_store_rate_limit = function(resp) {
      ## TODO TEST / HANDLE RATE LIMIT EXCEEDED
      headers <- resp_headers(resp)
      is_valid_rate_limit <-
        paste0("fitbit-rate-limit-", c("limit", "remaining", "reset")) |>
        map_lgl(~ !is.null(headers[[.x]])) |>
        all()
      if (is_valid_rate_limit) {
        rate_limit <- with(headers, list(
          limit = as.integer(`fitbit-rate-limit-limit`),
          remaining = as.integer(`fitbit-rate-limit-remaining`),
          reset = Sys.time() + as.integer(`fitbit-rate-limit-reset`)
        ))
        .env$rate_limiter$set(
          key = rate_limit_key,
          value = rate_limit
        )
      } else {
        cli_alert_warning("Missing or invalid Fitbit rate limit headers in response.")
      }
      resp
    },
    #' @description Limit the rate of API calls
    #' Now the response is just to wait, which is not ideal.
    limit_rate = function() {
      rate_limit <- .env$rate_limiter$get(rate_limit_key)
      if (!is.key_missing(rate_limit)) {
        if (rate_limit$remaining == 0) {
          wait <- rate_limit$reset - Sys.time()
          if (wait > 0) {
            cli_abort(
              "Rate limit exceeded. Wait for {round(wait)} seconds.",
              wait = wait,
              class = "fitbit_rate_limit_exceeded"
            )
          }
        }
      }
    },
    req_fitbit_error = function(req, error_message = NULL) {
      req_error(req, body = function(resp) {
        errors <- resp_body_json(resp)$errors
        sapply(errors, function(x) {
          c(
            error_message,
            glue("Fitbit API error of type: {x$errorType}"),
            glue("Message: {x$message}")
          )
        })
      })
    },
    #' @description Get the weight log
    #' @param start_date The start date
    #' @param end_date The end date. Use either end_date or period. Range can be at most 31 days.
    #' @param period The period of the log. One of "1d", "7d", "30d", "1w", "1m".
    body_log_weight = function(start_date,
                               end_date = NULL,
                               period = c("1d", "7d", "30d", "1w", "1m")) {
      start_date <- format(start_date, format = "%F")
      path <-
        if (is.null(end_date)) {
          if (missing(period)) {
            start_date
          } else {
            c(start_date, match.arg(period))
          }
        } else {
          c(start_date, format(end_date, format = "%F"))
        }
      q <- self$query(
        c("body/log/weight/date", path),
        error_message = glue("Failed to get weight log for user {self$token$user_id}.")
      )
      if (length(q$weight)) {
        bind_rows(q$weight) |>
          mutate(
            time = as.POSIXct(paste(date, time)),
            logId = as.character(logId),
            across(any_of(c("weight", "bmi", "fat")), as.double)
          ) |>
          select(-date) |>
          pivot_longer(-c(time, source, logId), names_to = "variable") |>
          select(variable, source, logId, time, value)
      } else {
        ## Return an empty tibble
        tibble(
          variable = character(),
          source = character(),
          logId = character(),
          time = as.POSIXct(character()),
          value = double())
      }
    },
    #' @description Get nutrition time series
    #' https://dev.fitbit.com/build/reference/web-api/nutrition-timeseries/get-nutrition-timeseries-by-date-range/
    #' @param start_date The start date
    #' @param end_date The end date. Use either end_date or period. Range can be at most 1095 days.
    #' @param period The period of the log. One of "1d", "7d", "30d", "1w", "1m", "3m", "6m", "1y".
    #' @param resource The resource to query. One of "caloriesIn", "water".
    nutrition_time_series = function(start_date,
                                     end_date = NULL,
                                     period = c("1d", "7d", "30d", "1w", "1m", "3m", "6m", "1y"),
                                     resource = c("caloriesIn", "water")) {
      start_date <- format(start_date, format = "%F")
      resource <- match.arg(resource)
      path <-
        if (is.null(end_date)) {
          c(start_date, match.arg(period))
        } else {
          c(start_date, format(end_date, format = "%F"))
        }
      q <- self$query(
        c("foods/log", resource, "date", path),
        error_message = glue("Failed to get nutrition time series for user {self$token$user_id}.")
      )
      data <- q[[paste0("foods-log-", resource)]]
      if (length(data)) {
        bind_rows(data) |>
          mutate(
            variable = resource,
            time = as.Date(dateTime),
            value = as.double(value)
          ) |>
          filter(value > 0) |>
          select(variable, time, value)
      } else {
        tibble(
          variable = character(),
          time = as.Date(character()),
          value = double()
        )
      }
    },
    #' @description Get activity time series
    #' https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/
    #' @param start_date The start date
    #' @param end_date The end date. Use either end_date or period. Range can be at most 1095 days, except for activityCalories which is 30 days.
    #' @param period The period of the log. One of "1d", "7d", "30d", "1w", "1m", "3m", "6m", "1y".
    #' @param resource The resource to query. One of "calories", "activityCalories", "caloriesBMR", "distance", "elevation", "floors", "minutesSedentary", "minutesLightlyActive", "minutesFairlyActive", "minutesVeryActive", "steps", "swimming-strokes".
    activity_time_series = function(start_date,
                                    end_date = NULL,
                                    period = c("1d", "7d", "30d", "1w", "1m", "3m", "6m", "1y"),
                                    resource = c("calories", "activityCalories", "caloriesBMR", "distance", "elevation", "floors", "minutesSedentary", "minutesLightlyActive", "minutesFairlyActive", "minutesVeryActive", "steps", "swimming-strokes")) {
      start_date <- format(start_date, format = "%F")
      resource <- match.arg(resource)
      path <-
        if (is.null(end_date)) {
          c(start_date, match.arg(period))
        } else {
          c(start_date, format(end_date, format = "%F"))
        }
      q <- self$query(
        c("activities", resource, "date", path),
        error_message = glue("Failed to get activity time series for user {self$token$user_id}.")
      )
      data <- q[[paste0("activities-", resource)]]
      if (length(data)) {
        bind_rows(data) |>
          mutate(
            variable = resource,
            time = as.Date(dateTime),
            value = as.double(value)
          ) |>
          select(variable, time, value)
      } else {
        tibble(
          variable = character(),
          time = as.Date(character()),
          value = double()
        )
      }
    },
    #' @description Get the intraday data
    #' @param date The date
    #' @param resource The resource to query. One of "calories", "distance", "elevation", "floors", "steps", "swimming-strokes".
    #' @param detail_level The detail level. One of "1min", "5min", "15min". Default is "15min".
    activity_intraday = function(date = Sys.Date(),
                                 resource = c("calories", "distance", "elevation", "floors", "steps", "swimming-strokes"),
                                 detail_level = c("15min", "5min", "1min")) {
      date <- format(date, format = "%F")
      resource <- match.arg(resource)
      detail_level <- match.arg(detail_level)
      q <- self$query(
        c("activities", resource, "date", date, "1d", detail_level),
        error_message = glue("Failed to get intraday data for user {self$token$user_id}.")
      )
      header <- q[[paste0("activities-", resource)]][[1]]
      intraday <- q[[paste0("activities-", resource, "-intraday")]]
      date <- as.Date(header$dateTime)
      data <- intraday$dataset
      if (length(data)) {
        data <-
          bind_rows(data) |>
          mutate(
            time = as.POSIXct(paste(date, time), tz = "UTC"),
            value = as.double(value)
          )
        if (resource == "calories") {
          data <-
            data |>
            rename(calories = value) |>
            pivot_longer(cols = -time, names_to = "variable")
        } else {
          data <-
            data |>
            mutate(
              variable = resource,
              value = as.double(value)
            )
        }
      } else {
        tibble(
          variable = character(),
          time = as.POSIXct(character(), tz = "UTC"),
          value = double()
        )
      }
      attr(data, "sum") <- as.double(header$value)
      attr(data, "datasetInterval") <- intraday$datasetInterval
      attr(data, "datasetType") <- intraday$datasetType
      data |> relocate(variable, time, value)
    },
    #' @description Create a subscription
    create_subscription = function(collection_path =
                                     c("activities", "body", "foods", "sleep",
                                       "userRevokedAccess", "all"),
                                   subscription_id) {
      collection_path <- match.arg(collection_path)
      if (collection_path == "all") {
        collection_path <- NULL
      }
      query(
        c(collection_path, "apiSubscriptions", subscription_id),
        method = "POST",
        error_message = glue("Failed to create subscription for user {self$token$user_id}.")
      )
    },
    #' @description Delete a subscription
    delete_subscription = function(collection_path =
                                     c("activities", "body", "foods", "sleep",
                                       "userRevokedAccess", "all"),
                                   subscription_id) {
      collection_path <- match.arg(collection_path)
      if (collection_path == "all") {
        collection_path <- NULL
      }
      query(
        c(collection_path, "apiSubscriptions", subscription_id),
        method = "DELETE",
        error_message = glue("Failed to delete subscription for user {self$token$user_id}.")
      )
    },
    #' @description List subscriptions
    list_subscriptions = function(collection_path =
                                    c("activities", "body", "foods", "sleep",
                                      "userRevokedAccess", "all")) {
      collection_path <- match.arg(collection_path)
      if (collection_path == "all") {
        collection_path <- NULL
      }
      q <- query(c(collection_path, "apiSubscriptions"))
      bind_rows(q$apiSubscriptions)
    }
  )
)

