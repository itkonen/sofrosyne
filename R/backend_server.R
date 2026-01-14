#' @import plumber
#' @export
backend_server <- function() {
  system_message("Starting backend server")
  connect_db()
  pr() |>
    ## Health check endpoint
    ## Used by load balancers to check if the server is alive
    pr_get("/health-check", function(req, res) {
      "OK"
    },
    serializer = serializer_text()
    ) |>
    ## Return Fitbit authorization URL for frontend
    ## Creates a PKCE and state, stores them in cache,
    ## and returns the URL leading the user to Fitbit authorization page.
    pr_get("/fitbit-auth-url", function(req, res) {
      if (!check_backend_access_token(req, res)) {
        return(res)
      }
      uid <- req$argsQuery$uid
      stopifnot(is.character(uid))
      ## Create Fitbit authorization state
      data <- create_fitbit_auth_state(uid)
      .env$pkce_cache$set(
        key = data$state,
        value = list(pkce = data$pkce, uid = uid)
      )
      system_message("Returning Fitbit authorization URL for user {uid}")
      list(url = data$url)
    },
    serializer = serializer_unboxed_json()
    ) |>
    ## Endpoint for Fitbit redirect uri
    ## Called by Fitbit after user authorizes the app
    ## Receives the authorization code and state from Fitbit,
    ## retrieves the PKCE and uid from cache,
    ## requests the access token from Fitbit,
    ## and creates subscriptions for the user.
    pr_get("/fitbit-auth-code", function(req, res) {
      if (!check_backend_access_token(req, res)) {
        return(res)
      }
      state <- req$argsQuery$state
      code <- req$argsQuery$code
      stopifnot(is.character(state), is.character(code))
      ## Get authorization state data from cache.
      ## The data contains the PKCE and the uid.
      data <- .env$pkce_cache$get(state)
      stopifnot(!is.null(data$pkce))
      .env$pkce_cache$remove(state)
      uid <- data$uid
      system_message(
        "Requesting Fitbit access token for user {uid}"
      )
      token <- FitbitToken$new(code = code, pkce = data$pkce, uid = uid)
      ## Subscribe to Fitbit notifications and store the subscription
      api <- FitbitWebAPI$new(token = token)
      walk(c("activities", "body", "foods"), function(collection) {
        api$create_subscription(
          collection_path = collection,
          subscription_id = paste(uid, collection, sep = "_")
        ) |>
          .env$db$upsert_fitbit_subscription(uid = uid)
      })
      res$status <- 204
      res
    }) |>
    ## Check if user is Fitbit authorized
    pr_get("/fitbit-authorized", function(req, res) {
      if (!check_backend_access_token(req, res)) {
        return(res)
      }
      uid <- req$argsQuery$uid
      stopifnot(is.character(uid))
      system_message("Checking Fitbit authorization for user {uid}")
      list(authorized = .env$db$is_fitbit_authorized(uid))
    },
    serializer = serializer_unboxed_json()
    ) |>
    ## Fitbit subscriber to receive notifications
    ## Called by Fitbit when there is new data for a user
    pr_post("/fitbit-subscriber", function(req, res) {
      if (handle_fibit_subscriber_verification(req, res)) {
        return(res)
      }
      if (!check_fitbit_notification_signature(req, res)) {
        system_message("Fitbit notification signature incorrect")
        return(res)
      }
      system_message("Received Fitbit notifications")
      later::later(function() {
        handle_fitbit_notifications(req)
      })
      res$status <- 204
      res
    }) |>
    ## Refresh Fitbit data for a user
    pr_get("/fitbit-refresh-data", function(req, res) {
      if (!check_backend_access_token(req, res)) {
        return(res)
      }
      uid <- req$argsQuery$uid
      stopifnot(is.character(uid))
      system_message("Refreshing Fitbit data for user {uid}")
      refresh_fitbit_data(uid)
      res$status <- 204
      res
    }) |>
    pr_set_error(function(req, res, err) {
      res$status <- 500
      list(error = err$message)
    }) |>
    pr_run(
      host = "0.0.0.0",
      port = getOption("sofrosyne.backend_port", 8000),
      docs = FALSE
    )
}

## Check backend access token
#' @keywords internal
check_backend_access_token <- function(req, res) {
  expect <- paste("Bearer", getOption("sofrosyne.backend_access_token"))
  if (is.null(req$HTTP_AUTHORIZATION) || req$HTTP_AUTHORIZATION != expect) {
    res$status <- 401
    FALSE
  } else {
    TRUE
  }
}

#' Check subscriber signature
#' https://dev.fitbit.com/build/reference/web-api/developer-guide/best-practices/#Subscriber-Security
#' @importFrom openssl sha1 base64_encode
#' @param req Request object
#' @keywords internal
check_fitbit_notification_signature <- function(req, res) {
  signature <- req$HTTP_X_FITBIT_SIGNATURE
  if (is.null(signature)) {
    system_message("Fitbit notification signature missing")
    res$status <- 404
    return(FALSE)
  }
  body <- req$bodyRaw
  signing_key <-
    paste0(getOption("sofrosyne.fitbit_client_secret"), "&")
  confirmation_signature <-
    sha1(body, key = signing_key) |>
    base64_encode()
  signature == confirmation_signature
}

#' Answer Fitbit subscriber verification request
#' @param req Request object
#' @keywords internal
handle_fibit_subscriber_verification <- function(req, res) {
  verify <- req$argsQuery$verify
  if (is.null(verify)) {
    FALSE
  } else {
    ## Check if verification code is correct
    if (Sys.getenv("FITBIT_VERIFICATION_CODE") == verify) {
      system_message("Fitbit subscriber verified: correct verification code")
      res$status <- 204L
    } else {
      system_message("Fitbit subscriber verified: incorrect verification code")
      res$body <- ""
      res$status <- 404L
    }
    TRUE
  }
}

#' Handle Fitbit notification
#' @param req Request object
#' @keywords internal
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom purrr pmap
#' @importFrom rlang try_fetch
handle_fitbit_notifications <- function(req) {
  req$body |>
    group_split(ownerId) |>
    walk(function(notifications) {
      user_id <- notifications$ownerId[1]
      uid <- str_extract(notifications$subscriptionId[1], "^[^_]+")
      token <- FitbitToken$new(uid = uid)
      api <- FitbitWebAPI$new(token)
      notifications |>
        select(collectionType, date) |>
        pmap(possibly(function(collectionType, date) {
          if (collectionType == "body") {
            api$body_log_weight(date) |>
              select(variable, time, value) |>
              drop_na(value)
          } else if (collectionType == "foods") {
            api$nutrition_time_series(date, resource = "caloriesIn") |>
              mutate(
                time = as.POSIXct(time, tz = "UTC"),
                variable = "calories_in"
              )
          } else if (collectionType == "activities") {
            bind_rows(
              api$activity_time_series(date, resource = "calories") |>
                mutate(variable = "calories_out"),
              api$activity_intraday(date, resource = "calories",
                                    detail_level = "15min") |>
                filter(variable == "calories") |>
                mutate(variable = "calories_out_intraday")
            )
          } else {
            NULL
          }
        }, quiet = FALSE)) |>
        bind_rows() |>
        .env$db$upsert_personal_data(uid = uid)
    })
  system_message("Fitbit notifications handled")
  NULL
}

#' Refresh fitbit data history
#' @param uid User ID
#' @keywords internal
refresh_fitbit_data <- function(uid) {
  system_message("Refreshing Fitbit data for user {uid}")
  token <- FitbitToken$new(uid = uid)
  api <- FitbitWebAPI$new(token)
  td <- today("UTC")
  ## Weight needs to be retrieved in batches of 31 days
  start_dates <- seq(td - 1095L, td, by = "31 day")
  end_dates <- pmin(start_dates + 30L, td)
  weight <- try_fetch({
    map2(start_dates, end_dates, api$body_log_weight) |>
      bind_rows() |>
      select(variable, time, value) |>
      drop_na(value)
  }, error = function(e) {
    system_message("Error retrieving weight data")
    NULL
  })
  calories_in <- try_fetch({
    api$nutrition_time_series(td - 1094L, td, resource = "caloriesIn") |>
      mutate(
        time = as.POSIXct(time, tz = "UTC"),
        variable = "calories_in"
      )
  }, error = function(e) {
    system_message("Error retrieving calories_in data")
    NULL
  })
  calories_out <- try_fetch({
    api$activity_time_series(td - 1094L, td, resource = "calories") |>
      mutate(variable = "calories_out")
  }, error = function(e) {
    system_message("Error retrieving calories_out data")
    NULL
  })
  ## Intrady calories out, retrieve 70 days in one day batches
  dates <- seq(td - 69L, td, by = "day")
  calories_out_intraday <- try_fetch({
    map(dates, ~ {
      api$activity_intraday(.x, resource = "calories", detail_level = "15min")
    }) |>
      bind_rows() |>
      filter(variable == "calories") |>
      mutate(variable = "calories_out_intraday")
  }, error = function(e) {
    system_message("Error retrieving calories_out_intraday data")
    NULL
  })
  bind_rows(weight, calories_in, calories_out, calories_out_intraday) |>
    .env$db$upsert_personal_data(uid = uid)
}
