#!/usr/bin/env Rscript

## library("optparse")

## args <-
##   OptionParser() |>
##   add_option(c("-s", "--service"),
##     type = "character",
##     help = "Kubernetes data server service name"
##   ) |>
##   add_option(c("-p", "--production"),
##     action = "store_true", default = FALSE,
##     help = "Golem production mode"
##   ) |>
##   parse_args()

## if (is.null(args$service)) {
##   options(
##     shiny.port = 8080,
##     shiny.host = "0.0.0.0"
##   )
## } else {
##   service <- toupper(args$service)
##   service <- gsub("-", "_", service)
##   host <- Sys.getenv(paste0(service, "_SERVICE_HOST"))
##   port <- Sys.getenv(paste0(service, "_SERVICE_PORT"))
##   Sys.setenv(ROBONOMIST_SERVER = paste(host, port, sep = ":"))
##   options(
##     shiny.port = 80,
##     shiny.host = "0.0.0.0"
##   )
## }

## options(
##   shiny.port = 8080,
##   shiny.host = "0.0.0.0"
## )

cat("Starting Sofrosyne client...\n")
sofrosyne::run_app()
