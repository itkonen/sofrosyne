mod_value_boxes_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("value_boxes"))
}

mod_value_boxes_server <- function(id, model, rdb) {
  moduleServer(id, function(input, output, session) {

    muted_span <- function(...) {
      span(..., class = "text-muted ms-1")
    }

    recent_data <- reactive({
      req(model$main_output()) |>
        filter(time %in% seq(Sys.Date() - 8, Sys.Date(), by = "days"))
    })

    weight_today_box <- reactive({
      weight <-
        recent_data() |>
        filter(variable == "weight", type == "state") |>
        filter(time %in% c(Sys.Date() - 7, Sys.Date() - 1, Sys.Date())) |>
        with(
          list(
            today = paste0(round(value[3], 2), " kg"),
            change_1 = paste0(round(1000 * (value[3] - value[2])), " g/pv"),
            change_7 = paste0(round(1000 * (value[3] - value[1])), " g/vko")
          )
        )
      weight_observation <-
        recent_data() |>
        filter(variable == "weight", type == "observation") |>
        filter(time == max(time))
      value_box(
        title = "Paino tänään",
        value = p(weight$today, muted_span(glue("({weight$change_1})"))),
        showcase = icon("weight-scale"),
        p("Viikko muutos: ", weight$change_7),
        p("Punnitus: ", weight_observation$value),
        theme = "green"
      )
    })

    calorie_balance_yesterday_box <- reactive({
      balance <-
        recent_data() |>
        filter(type == "state", time != Sys.Date()) |>
        pivot_wider(names_from = variable, values_from = value) |>
        with({
          balance <- calories_in - calories_out
          list(
            calories_in = round(last(calories_in), 0),
            calories_out = round(last(calories_out), 0),
            balance = round(last(balance), 0),
            balance_7 = round(sum(calories_in - calories_out), 0)
          )
        })
      value_box(
        title = "Kaloritase eilen",
        value = balance |>
          glue_data("{calories_in} – {calories_out} = {balance}"),
        showcase = icon("scale-unbalanced"),
        p("Viikkotase: ", balance$balance_7, " kcal"),
        theme = "orange"
      )
    })

    calorie_target_box <- reactive({
      intraday <-
        model$intraday_output() |>
        pivot_wider(names_from = variable, values_from = value) |>
        summarize(
          time = as.Date(time[1]),
          typical_day = round(sum(`Typical day`)),
          current_day_forecast = round(
            sum(`Observation`, na.rm = TRUE) + sum(`Forecast`, na.rm = TRUE)
          )
        )
      balance_target <- 0
      calorie_in_target <-
        round(intraday$current_day_forecast + balance_target)
      calories_in_so_far <-
        rdb$calories_in_today() |>
        filter(time == intraday$time) |>
        pull(value)
      if (length(calories_in_so_far) == 0) calories_in_so_far <- 0
      calories_left <-
        calorie_in_target - calories_in_so_far
      value_box(
        title = tagList(
          p("Kaloreita tänään: ", muted_span(glue("{calories_in_so_far} kcal"))),
          p("Jäljellä: ", muted_span(glue("{calories_left} kcal")))
        ),
        value = p(
          tooltip(intraday$current_day_forecast, "Ennustettu kulutus"),
          icon("arrow-right"),
          tooltip(
            span(glue("{calorie_in_target}"), class = "text-muted ms-2"),
            "Ravintotavoite"
          )
        ),
        showcase = icon("fire"),
        theme = "blue",
        p(
          format(Sys.Date(), "Tyypillinen %A:"),
          muted_span(glue("{intraday$typical_day} kcal"))
        )
      )
    })

    weight_forecast_box <- reactive({
      forecast <-
        req(model$main_output()) |>
        filter(time > Sys.Date(), type == "state") |>
        pivot_wider(names_from = variable, values_from = value) |>
        summarise(
          time = last(time),
          change = round(
            (last(weight) - first(weight)) / (n() - 1) * 7 * 1000
          ),
          weight = round(last(weight), 1),
          calories_in = mean(calories_in),
          calories_out = mean(calories_out)
        )
      value_box(
        title = p("Ennuste", muted_span(format(forecast$time, "%-d.%m.%Y"))),
        value = p(
          forecast$weight, " kg",
          muted_span(glue("({forecast$change} g/vko)")
          )
        ),
        showcase = icon("arrow-trend-down"),
        p("Ravinto: ", round(forecast$calories_in), " kcal"),
        p("Kulutus: ", round(forecast$calories_out), " kcal"),
        theme = "teal"
      )
    })

    output$value_boxes <- renderUI({
      one_per_flush()
      system_message("Rendering value boxes")

      layout_column_wrap(
        weight_today_box(),
        calorie_balance_yesterday_box(),
        calorie_target_box(),
        weight_forecast_box()
      )
    })
  })
}
