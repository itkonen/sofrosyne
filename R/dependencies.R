#' @importFrom roboplotr set_modebar
sofrosyne_dependencies <- function(tag) {
  tag |>
    tagList(
      useBusyIndicators(),
      set_roboplot_options(
        height = 450,
        shinyapp = TRUE,
        logo = "none",
        trace_colors = color_palette,
        caption_template = "",
        verbose = "Warning",
        modebar = set_modebar(display = "none")
      ),
      htmlDependency(
        "sofrosyne",
        version = "0.0.1",
        src = c(file = system.file("", package = "sofrosyne")),
        script = "sofrosyne.js"
      )
    )
}
