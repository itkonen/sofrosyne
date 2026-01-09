#' @export
plot <- function(cd = combined_data()) {
  cd |>
    ggplot(aes(time, value, color = variable, alpha = type)) +
    geom_line(data = cd |> filter(type != "observation")) +
    geom_point(data = cd |> filter(type == "observation")) +
    facet_wrap(~variable, scales = "free_y") +
    scale_alpha_manual(values = c(observation = 0.7, state = 1, forecast = 0.4)) +
    scale_x_date(date_breaks = "months", date_labels = "%b") ## +
    ## scale_y_continuous(breaks = seq(0, 100, by = 2))
}

## ## #' @export
## ## plot_weight <- function(cd = combined_data()) {
## ##   cd <- cd |> filter(variable == "weight")
## ##   cd |>
## ##     ggplot(aes(time, value, color = variable, alpha = type)) +
## ##     geom_line(data = cd |> filter(type != "observation")) +
## ##     geom_point(data = cd |> filter(type == "observation")) +
## ##     scale_alpha_manual(values = c(observation = 0.7, state = 1, forecast = 0.4)) +
## ##     scale_x_date(date_breaks = "months", date_labels = "%b")
## ## }

## #' @export
## plot_weight <- function(cd = combined_data()) {
##   d <- cd |> filter(variable == "weight")
##   sr <-
##     cd |>
##     filter(type == "state", variable %in% c("weight", "retention")) |>
##     summarize(.by = time, value = sum(value), variable = "sr_weight", type = "state")

##   d |>
##     ggplot(aes(time, value, color = variable, alpha = type)) +
##     geom_line(data = d |> filter(type %in% c("state", "forecast"))) +
##     geom_line(data = sr) +
##     geom_point(data = cd |> filter(type == "observation", variable == "weight")) +
##     scale_alpha_manual(values = c(observation = 0.7, state = 1, forecast = 0.4)) +
##     scale_x_date(date_breaks = "months", date_labels = "%b") +
##     scale_y_continuous(breaks = seq(70, 100, by = 2)) +
##     labs(x= NULL, y= NULL)
## }

## #' @export
## plot_change <- function(cd = combined_data()) {
##   cd |>
##     filter(variable == "weight") |>
##     complete(time, type, variable) |>
##     mutate(
##       value = value - lag(value),
##       .by = "type"
##     ) |>
##     filter(type == "state") |>
##     ggplot(aes(time, value, color = type)) +
##     geom_line() +
##     geom_point() +
##     geom_hline(yintercept = 0)
## }

## #' @export
## plot_change_2 <- function(cd = combined_data()) {
##   cd |>
##     filter(
##       variable == "weight",
##       type == "state"
##     ) |>
##     mutate(
##       value = value - lag(value, 7),
##       ## average = slider::slide_dbl(value, mean, .before = 6),
##       .by = "type"
##     ) |>
##     ggplot(aes(time, value, color = type)) +
##     geom_line() +
##     ## geom_line(aes(y = average), color = "red") +
##     geom_hline(yintercept = 0)
## }
