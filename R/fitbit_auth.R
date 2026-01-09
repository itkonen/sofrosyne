
#' Fitbit client
#' @import httr2
fitbit_client <- function() {
  oauth_client(
    id = getOption("sofrosyne.fitbit_client_id"),
    secret = getOption("sofrosyne.fitbit_client_secret"),
    token_url = "https://api.fitbit.com/oauth2/token",
    name = "sofrosyne",
    auth = "header"
  )
}

req_fitbit_error <- function(req) {
  req_error(req, body = function(resp) {
    errors <- resp_body_json(resp)$errors
    sapply(errors, function(x) {
      c(
        glue("Fitbit API error of type: {x$errorType}"),
        glue("Message: {x$message}")
      )
    })
  })
}

#' Fitbit authorization URL with PKCE
#'
#' NOTE: With local memory cache, this is works only in a single R session.
#' @param uid User ID.
#' @return Fitbit authorization URL.
#' @importFrom openssl sha256
create_fitbit_auth_state <- function(uid) {
  ## Create a state string for OAuth, this is sent to Fitbit by the user
  ## and returned in the callback. Its hash is then used as a key to store
  ## the PKCE and the user ID for retrieving the token.
  state <- as.character(sha256(
    paste("pkce/state/token", uid),
    key = "54bd82b777645ff31548c"
  ))
  pkce <- oauth_flow_auth_code_pkce()
  list(
    state = state,
    pkce = pkce,
    url = make_oauth_url(pkce, state)
  )
}
#' Fitbit authorization URL
#' @param pkce PKCE.
#' @param state User specific state sent to authorization.
#' @return Fitbit authorization URL.
#' @importFrom httr2 oauth_flow_auth_code_url
make_oauth_url <- function(pkce, state) {
  oauth_flow_auth_code_url(
    client = .env$fitbit_client,
    auth_url = "https://www.fitbit.com/oauth2/authorize",
    redirect_uri = getOption("sofrosyne.fitbit_redirect_uri"),
    scope = "weight nutrition activity",
    state = state,
    auth_params = list(
      expires_in = 2592000,
      code_challenge = pkce$challenge,
      code_challenge_method = pkce$method
    )
  )
}


#' Get token from Fitbit
#' @param code Authorization code.
#' @param pkce PKCE.
#' @return Token.
#' @importFrom openssl base64_encode
#' @importFrom rlang current_call
#' @import httr2
request_fitbit_access_token <- function(code, pkce) {
  client <- .env$fitbit_client
  req <-
    request(client$token_url) |>
    req_body_form(
      client_id = client$id,
      code = code,
      code_verifier = pkce$verifier,
      grant_type = "authorization_code",
      redirect_uri = getOption("sofrosyne.fitbit_redirect_uri")
    ) |>
    oauth_client_req_auth(client) |>
    req_fitbit_error()
  resp <- req_perform(req, error_call = current_call())
  do.call(oauth_token, resp_body_json(resp))
}

#' Refresh token, not working
#' @param token Token.
refresh_fitbit_access_token <- function(token) {
  client <- .env$fitbit_client
  req <-
    request(client$token_url) |>
    req_body_form(
      grant_type = "refresh_token",
      refresh_token = token$refresh_token
    ) |>
    oauth_client_req_auth(client) |>
    req_fitbit_error()
  resp <- req_perform(req, error_call = current_call())
  do.call(oauth_token, resp_body_json(resp))
}

#' Fitbit token
#' @importFrom lubridate now
FitbitToken <- R6Class(
  "FitbitToken",
  portable = FALSE,
  cloneable = FALSE,
  active = list(
    #' @field access_token Returns the access token, refreshing if necessary.
    access_token = function(value) {
      if (missing(value)) {
        refresh()
        self$.access_token
      } else {
        self$.access_token <- value
      }
    }
  ),
  public = list(
    #' @field token_type Type
    token_type = "Bearer",
    #' @field .access_token Access token
    .access_token = NULL,
    #' @field expires_at Expiration time
    expires_at = NULL,
    #' @field refresh_token Refresh token
    refresh_token = NULL,
    #' @field scope Scope
    scope = NULL,
    #' @field user_id Fitbit user ID
    user_id = NULL,
    #' @field uid Firebase user ID
    uid = NULL,
    #' @description Initialize Fitbit token.
    #' @param code Authorization code.
    #' @param pkce PKCE.
    #' @param uid Firebase user ID.
    #' @param test Test mode.
    initialize = function(code = NULL, pkce = NULL, uid = NULL,
                          test = FALSE) {
      if (is.null(.env$db)) {
        stop("Database not initialized.")
      }
      if (!is.null(code) && !is.null(pkce)) {
        ## Get token from Fitbit
        client <- .env$fitbit_client
        req <-
          request(client$token_url) |>
          req_body_form(
            client_id = client$id,
            code = code,
            code_verifier = pkce$verifier,
            grant_type = "authorization_code",
            redirect_uri = getOption("sofrosyne.fitbit_redirect_uri")
          ) |>
          oauth_client_req_auth(client) |>
          req_fitbit_error()
        resp <-
          req_perform(req, error_call = current_call()) |>
          resp_body_json()
        .access_token <<- resp$access_token
        expires_at <<- now("UTC") + resp$expires_in
        refresh_token <<- resp$refresh_token
        scope <<- resp$scope
        user_id <<- resp$user_id
        uid <<- uid
        .env$db$upsert_fitbit_token(self)
      } else if (test) {
        token <- get_test_token()
        .access_token <<- token$access_token
        expires_at <<- as.POSIXct(token$expires_at, tz = "UTC")
        refresh_token <<- token$refresh_token
        scope <<- token$scope
        user_id <<- token$user_id
        uid <<- uid
        .env$db$upsert_fitbit_token(self)
      } else {
        ## Get token from database
        token <- .env$db$get_fitbit_token(uid = uid)
        .access_token <<- token$access_token
        expires_at <<- token$expires_at
        refresh_token <<- token$refresh_token
        scope <<- token$scope
        user_id <<- token$user_id
        uid <<- token$uid
      }
    },
    #' @description Check if token is expired.
    is_expired = function() {
      now("UTC") > expires_at
    },
    #' @description Refresh token.
    #' @param force Force refresh.
    refresh = function(force = FALSE) {
      if (!(force || self$is_expired())) {
        return()
      }
      system_message("Refreshing Fitbit token for user {uid}")
      client <- .env$fitbit_client
      req <-
        request(client$token_url) |>
        req_body_form(
          grant_type = "refresh_token",
          refresh_token = self$refresh_token
        ) |>
        oauth_client_req_auth(client) |>
        req_fitbit_error()
      resp <-
        req_perform(req, error_call = current_call()) |>
        resp_body_json()
      .access_token <<- resp$access_token
      expires_at <<- now("UTC") + resp$expires_in
      refresh_token <<- resp$refresh_token
      .env$db$upsert_fitbit_token(self)
      TRUE
    }
  )
)

get_test_token <- function() {
  oauth_flow_auth_code(
    client = .env$fitbit_client,
    auth_url = "https://www.fitbit.com/oauth2/authorize",
    redirect_uri = "https://localhost:8080",
    scope = "weight nutrition activity"
  )
}
