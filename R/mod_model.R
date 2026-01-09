mod_model_server <- function(id, reactive_db) {
  moduleServer(id, function(input, output, session) {

    main_input <- reactive({
      system_message(
        "Retrieving main model data from db for user {reactive_db$uid}"
      )
      req(reactive_db$health_data()) |>
        sofrosyne_model_interface()
    })

    main_output <- reactive({
      system_message(
        "Calculating main output for user {reactive_db$uid}"
      )
      d <- SofrosyneModel$new(main_input())$combine_data()
      attr(d, "frequency") <- "Daily"
      d
    })

    intraday_output <- reactive({
      system_message(
        "Calculating intraday model for user {reactive_db$uid}"
      )
      calories_out_state_today <-
        main_output() |>
        filter(
          type == "state",
          variable == "calories_out_mean",
          time == today() ## tz?
        ) |>
        pull(value)
      req(reactive_db$health_data()) |>
        intraday_model_interface() |>
        intraday_model() |>
        mutate(
          value = value * calories_out_state_today
        )
    })

    list(
      main_input = main_input,
      main_output = main_output,
      intraday_output = intraday_output
    )
  })
}
