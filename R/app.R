#' @import shiny bslib
#' @import dplyr tibble tidyr purrr lubridate stringr forcats
#' @import cli
#' @importFrom utils tail
#' @importFrom roboplotr set_roboplot_options
#' @importFrom htmltools htmlDependency
authorized_ui <- function(req) {
  page_navbar(
    title = "Sofrosyne",
    theme = app_theme(),
    nav_panel(
      "Etusivu",
      icon = icon("home"),
      uiOutput("main")
    ),
    nav_spacer(),
    nav_item(
      actionButton("refresh_data", NULL,
                   icon = icon("refresh"), class = "btn-sm")
    ),
    nav_item(
      actionButton("authorize_fitbit", NULL,
                   icon = icon("link"), class = "btn-sm")
    ),
    nav_item(
      actionButton("sign_out", NULL,
                   icon = icon("sign-out-alt"), class = "btn-sm")
    )
  ) |>
    sofrosyne_dependencies()
}

#' Handle Fitbit OAuth redirect callback and forward code/state to backend.
#' Returns a UI object (script redirect) or NULL if not applicable/invalid.
handle_fitbit_redirect_ui <- function(req) {
  query <- parseQueryString(req$QUERY_STRING)
  if (!is.null(query$code) && !is.null(query$state)) {
    request(getOption("sofrosyne.backend_url")) |>
      req_url_path("fitbit-auth-code") |>
      req_headers(
        Authorization =
          paste("Bearer", getOption("sofrosyne.backend_access_token"))
      ) |>
      req_url_query(
        code = query$code,
        state = query$state
      ) |>
      req_perform()

    ## TODO: This should be asynchronous, but there's a possible race
    ## condition if the user is redirected before the backend has processed
    ## the authorization code (and cannot access the data yet).
    system_message(
      "Fitbit authorization sent to backend. Redirecting user to main UI"
    )
    return(tags$script(HTML("location.replace('/');")))
  }

  system_message("Invalid query string in Fitbit redirect URI")
  NULL
}

#' @import aegis
#' @importFrom htmltools HTML
ui <- function(req) {
  if (req$PATH_INFO == "/fitbit-redirect-url") {
    ## Catch Fitbit authorization code and send it to the backend
    handle_fitbit_redirect_ui(req)
  } else {
    ## Serve the main UI
    aegis_ui(authorized_ui)(req)
  }
}

#' @importFrom rlang hash
#' @importFrom cachem cache_disk is.key_missing
#' @importFrom openssl base64_encode sha1
#' @importFrom glue glue_data glue
server <- function(input, output, session) {

  aegis <- aegis_server(
    providers = "google",
    authorize_user = function(user) {
      user$email == "itkonen.juha@gmail.com"
    }
  )

  observeEvent(input$sign_out, {
    system_message("User {aegis$user()$uid} signed out")
    aegis$sign_out()
  })

  observeEvent(aegis$is_authorized(), {
    req(aegis$is_authorized())
    system_message(
      "User {aegis$user()$uid} entered the authorized site"
    )

    output$main <- renderUI({
      tagList(
        mod_value_boxes_ui("value_boxes"),
        mod_plots_ui("plots")
      )
    })

    fitbit <- mod_fitbit_server(
      "fitbit",
      user = aegis$user,
      redirect_to_authorization = reactive(input$authorize_fitbit),
      refresh_data = reactive(input$refresh_data)
    )

    rdb <- mod_reactive_db_server(
      "db",
      uid = aegis$user()$uid,
      con = con,
      refresh_data = reactive(input$refresh_data)
    )

    model <- mod_model_server("model", rdb)

    mod_value_boxes_server("value_boxes", model, rdb)
    mod_plots_server("plots", model)
  })

}

#' @export
run_app <- function() {
  system_message("Starting Sofrosyne app")
  if (is.null(getOption("sofrosyne.db_password"))) {
    stop("SOFROSYNE_DB_PASSWORD not set")
  }
  connect_db()
  shinyApp(
    ui = ui,
    server = server,
    uiPattern = "(/|/fitbit-redirect-url)",
    onStart = function() {
      onStop(function() {
        rm(db, envir = .env)
        gc()
      })
    }
  )
}
