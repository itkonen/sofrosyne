#' @importFrom cachem cache_mem
.onLoad <- function(...) {
  .env$rate_limiter <- cachem::cache_mem()
  options(
    shiny.port = getOption("shiny.port", 8080),
    shiny.host = "0.0.0.0",
    ## shiny.cache = cache_disk("cache"),
    plumber.docs = FALSE,
    sofrosyne.db_host =
      Sys.getenv("SOFROSYNE_DB_HOST", "localhost"),
    sofrosyne.db_user =
      Sys.getenv("SOFROSYNE_DB_USER", "frontend"),
    sofrosyne.db_password =
      Sys.getenv("SOFROSYNE_DB_PASSWORD"),
    sofrosyne.fitbit_client_id =
      Sys.getenv("SOFROSYNE_FITBIT_CLIENT_ID"),
    sofrosyne.fitbit_client_secret =
      Sys.getenv("SOFROSYNE_FITBIT_CLIENT_SECRET"),
    sofrosyne.backend_url =
      Sys.getenv("SOFROSYNE_BACKEND_URL", "http://localhost:8000"),
    sofrosyne.backend_port = 8000,
    sofrosyne.frontend_url =
      Sys.getenv("SOFROSYNE_FRONTEND_URL", "http://localhost:8080"),
    ## Used to request redirect_uri from the backend
    sofrosyne.backend_access_token = "ln97|lknlgkslkngdrrvvd",
    cli.condition_width = Inf
  )
  options(
    sofrosyne.fitbit_redirect_uri = paste0(
      getOption("sofrosyne.frontend_url"), "/fitbit-redirect-url"
    )
  )
  .env$pkce_cache <- cachem::cache_mem(
    max_size = 5 * 1024^2,
    max_age = 60 * 60 * 2
  )
  .env$fitbit_client <- fitbit_client()
  packageStartupMessage(
    "Sofrosyne backend URL: ", getOption("sofrosyne.backend_url"), "\n",
    "Sofrosyne frontend URL: ", getOption("sofrosyne.frontend_url"), "\n",
    "Sofrosyne db host: ", getOption("sofrosyne.db_host"), "\n",
    "Fitbit client id: ", getOption("sofrosyne.fitbit_client_id")
  )
  ## reg.finalizer(.env, close_db, onexit = TRUE)
}

.env <- new.env(parent = emptyenv())

#' @importFrom pool dbPool
connect_db <- function() {
  .env$db <- SofrosyneDB$new()
}

## close_db <- function(env = .env) {
##   if (!is.null(env$db)) {
##     cat("Closing database connection\n")
    
##   }
## }
