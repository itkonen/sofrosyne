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

#' @import pergamos
#' @importFrom htmltools HTML
ui <- function(req) {
  ## Catch Fitbit authorization code and send it to the backend
  if (req$PATH_INFO == "/fitbit-redirect-url") {
    query <- parseQueryString(req$QUERY_STRING)
    if (!is.null(query$code) && !is.null(query$state)) {
      url <- paste0(getOption("sofrosyne.backend_url"), "/fitbit-auth-code")
      request(url) |>
        req_headers(
          Authorization =
            paste("Bearer", getOption("sofrosyne.backend_access_token"))
        ) |>
        req_url_query(
          code = query$code,
          state = query$state
        ) |>
        req_perform()
      system_message(
        "Fitbit authorization sent to backend. Redirecting user to main UI"
      )
      tags$script(
        HTML("location.replace('/');")
      )
    } else {
      system_message("Invalid query string in Fitbit redirect URI")
      NULL
    }
  } else {
    ## Serve the main UI
    secure_ui(authorized_ui = authorized_ui)(req)
  }
}

#' @importFrom rlang hash
#' @importFrom cachem cache_disk is.key_missing
#' @importFrom openssl base64_encode sha1
#' @importFrom glue glue_data glue
server <- function(input, output, session) {

  ss <- secure_server(authorize_user_fn = function(user) {
    user$email == "itkonen.juha@gmail.com"
  })

  observeEvent(input$sign_out, {
    system_message("User {ss$user()$uid} signed out")
    ss$sign_out()
  })

  observeEvent(ss$authorization(), {
    req(ss$authorization())
    system_message(
      "User {ss$user()$uid} entered the authorized site"
    )

    output$main <- renderUI({
      tagList(
        mod_value_boxes_ui("value_boxes"),
        mod_plots_ui("plots")
      )
    })

    fitbit <- mod_fitbit_server(
      "fitbit",
      user = ss$user,
      redirect_to_authorization = reactive(input$authorize_fitbit),
      refresh_data = reactive(input$refresh_data)
    )

    rdb <- mod_reactive_db_server(
      "db",
      uid = ss$user()$uid,
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
  pergamos::set_cookie_key("sofrosyne")
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
