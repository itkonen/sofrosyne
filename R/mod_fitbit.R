backend_query <- function(uid, endpoint) {
  url <- paste0(getOption("sofrosyne.backend_url"), endpoint)
  request(url) |>
    req_headers(
      Authorization = paste("Bearer", getOption("sofrosyne.backend_access_token"))
    ) |>
    req_url_query(uid = uid) |>
    req_perform()
}

mod_fitbit_server <- function(id,
                              user,
                              redirect_to_authorization,
                              refresh_data) {
  moduleServer(id, function(input, output, session) {

    ## Redirect to Fitbit for authorization
    observeEvent(
      redirect_to_authorization(),
      {
        req(user()$uid)
        system_message(
          "Redirecting user {user()$uid} to Fitbit for authorization"
        )
        ## Query sofrosyne backend for Fitbit authorization URL
        resp <-
          backend_query(user()$uid, "/fitbit-auth-url") |>
          resp_body_json()
        url <- resp$url[[1]]
        session$sendCustomMessage("redirect-to-url", url)
      }, ignoreInit = TRUE
    )

    is_authorized <- reactive({
      req(user()$uid)
      system_message(
        "Checking Fitbit authorization for user {user()$uid}"
      )
      resp <-
        backend_query(user()$uid, "/fitbit-authorized") |>
        resp_body_json()
      resp$authorized[[1]]
    })

    observeEvent(
      refresh_data(),
      {
        req(user()$uid)
        system_message(
          "Refreshing Fitbit data for user {user()$uid}"
        )
        backend_query(user()$uid, "/fitbit-refresh-data")
      }, ignoreInit = TRUE
    )

    ## fitbit_api <- reactive({
    ##   req(user()$email, token())
    ##   Fitbit$new(token())
    ## })

    ## etl_main_model_data <- function(db, force = FALSE) {
    ##   req(fitbit_api())
    ##   system_message(
    ##     "Extracting Fitbit main model data for user {fitbit_api()$token$user_id}"
    ##   )
    ##   fitbit_api()$etl_main_model_data(db, force = force)
    ## }

    ## etl_intraday_data <- function(db, force = FALSE) {
    ##   req(fitbit_api())
    ##   system_message(
    ##     "Extracting Fitbit intraday model data for user {fitbit_api()$token$user_id}"
    ##   )
    ##   fitbit_api()$etl_intraday_data(db, force = force)
    ## }

    list(
      is_authorized = is_authorized()
      ## etl_main_model_data = etl_main_model_data,
      ## etl_intraday_data = etl_intraday_data
    )
  })
}
