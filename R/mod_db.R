mod_reactive_db_server <- function(id, uid, con, refresh_data) {
  moduleServer(id, function(input, output, session) {
    ## db <- SofrosyneDB$new()

    ## trigger <- reactiveVal(0)

    health_data <- reactive({
      ## trigger()
      ## update_data()
      .env$db$get_personal_data(uid)
    })

    calories_in_today <- reactive({
      health_data() |>
        filter(variable == "calories_in") |>
        filter(time == max(time, na.rm = TRUE)) |>
        transmute(
          time = as.Date(time),
          value
        )
    })

    list(
      uid = uid,
      health_data = health_data,
      calories_in_today = calories_in_today
      ## update_data = update_data
    )
  })
}
