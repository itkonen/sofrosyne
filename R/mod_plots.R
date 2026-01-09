mod_plots_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = "500px",
    height = "450px",
    fillable = FALSE,
    card(
      ## TODO: full_screen = TRUE,
      card_body(
        min_height = "450px",
        uiOutput(ns("weight_trend"))
      )
    ),
    card(
      card_body(
        min_height = "450px",
        uiOutput(ns("weight_change"))
      )
    ),
    card(
      card_body(
        min_height = "450px",
        uiOutput(ns("intraday"))
      )
    ),
    card(
      card_body(
        min_height = "450px",
        uiOutput(ns("balance"))
      )
    ),
    card(
      card_body(
        min_height = "450px",
        uiOutput(ns("fat"))
      )
    )
  )
}

#' @importFrom roboplotr roboplot set_shadearea set_axes
#' @importFrom scales muted
mod_plots_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$weight_trend <- renderUI({
      one_per_flush()
      system_message("Rendering weight trend")
      req(model$main_output()) |>
        filter(type == "state", variable %in% c("weight", "retention")) |>
        pivot_wider(names_from = variable) |>
        transmute(
          time,
          Painotrendi = weight,
          `Hetkellinen paino` = weight + retention
        ) |>
        pivot_longer(-time, names_to = "variable", values_to = "value") |>
        roboplot(
          variable,
          title = "Paino",
          caption = "Sofrosyne",
          rangeslider = Sys.Date() - 90,
          shadearea = set_shadearea(
            xmin = Sys.Date() + 1,
            xmax = Sys.Date() + 35
          ),
          xaxis_ceiling = Sys.Date() + 35
        )
    })

    output$fat <- renderUI({
      one_per_flush()
      system_message("Rendering fat")
      req(model$main_output()) |>
        filter(variable == "fat", type %in% c("state", "observation")) |>
        mutate(
          type = type |> recode("state" = "Rasvaprosentti", "observation" = "Mittaus")
        ) |>
        roboplot(
          type,
          title = "Rasvaprosentti",
          caption = "Sofrosyne",
          plot_mode = c("Rasvaprosentti" = "line", "Mittaus" = "scatter"),
          rangeslider = Sys.Date() - 90,
          shadearea = set_shadearea(
            xmin = Sys.Date() + 1,
            xmax = Sys.Date() + 35
          ),
          xaxis_ceiling = Sys.Date() + 35
        )
    })

    output$weight_change <- renderUI({
      one_per_flush()
      system_message("Rendering weight change")
      req(model$main_output()) |>
        filter(type == "state", variable == "weight") |>
        mutate(
          label = "Painon päivämuutos",
          value = 1000 * (value - lag(value, order_by = time))
        ) |>
        drop_na() |> ## BUG: Roboplot bar type ei osaa käsitellä NA-arvoja
        roboplot(
          label,
          title = "Painon päivämuutos",
          caption = "Sofrosyne",
          ## plot_type = "bar",
          rangeslider = Sys.Date() - 90,
          shadearea = set_shadearea(
            xmin = Sys.Date() + 1,
            xmax = Sys.Date() + 35
          ),
          xaxis_ceiling = Sys.Date() + 35
          ## Feature request: Roboplot ei osaa käsitellä xaxis_ceilingiä bar-tyypillä
        )
    })

    output$intraday <- renderUI({
      one_per_flush()
      system_message("Rendering intraday")
      d <- req(model$intraday_output())
      connect_lines <-
        d |>
        filter(variable == "Observation") |>
        filter(time == max(time)) |>
        mutate(variable = "Forecast")
      label <- format(Sys.Date(), "Tyypillinen %A")
      bind_rows(connect_lines, d) |>
        mutate(
          variable = variable |> recode(
            "Observation" = "Toteutunut",
            "Forecast" = "Loppupäivän ennuste",
            "Typical day" = label
          ) |>
            fct_relevel("Toteutunut", "Loppupäivän ennuste", label)
        ) |>
        roboplot(
          variable,
          title = "Päivän kalorikulutus",
          caption = "Sofrosyne",
          plot_mode = "step",
          plot_axes = set_axes(xformat = "%H:%M"),
          trace_color = setNames({
            p <- unname(color_palette[c(1, 1, 2)])
            p[2] <- muted(p[2], 90)
            p
          }, c("Toteutunut", "Loppupäivän ennuste", label))
        )
    })

    output$balance <- renderUI({
      one_per_flush()
      system_message("Rendering balance")
      req(model$main_output()) |>
        filter(
          variable %in% c(
            "calories_in",
            "calories_out",
            "calories_in_mean",
            "calories_out_mean"
          ),
          type == "state"
        ) |>
        mutate(
          variable = variable |> recode(
            "calories_in" = "Ravinto",
            "calories_out" = "Kulutus",
            "calories_in_mean" = "Ravinnon trendi",
            "calories_out_mean" = "Kulutuksen trendi"
          ) |> fct_relevel("Ravinnon trendi", "Ravinto",
                           "Kulutuksen trendi", "Kulutus")
        ) |>
        arrange(time) |>
        roboplot(
          variable,
          title = "Kaloritase",
          caption = "Sofrosyne",
          rangeslider = Sys.Date() - 90,
          shadearea = set_shadearea(
            xmin = Sys.Date() + 1,
            xmax = Sys.Date() + 35
          ),
          xaxis_ceiling = Sys.Date() + 35,
          trace_color = {
            p <- unname(color_palette)
            c(
              "Ravinto" = muted(p[1], 90),
              "Kulutus" = muted(p[2], 90),
              "Ravinnon trendi" = p[1],
              "Kulutuksen trendi" = p[2]
            )
          }
        )
    })
  })
}

