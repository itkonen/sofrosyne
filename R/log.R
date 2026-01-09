#' @import cli
system_message <- function(message, ...) {
  cli_inform(
    c("*" = paste(format(Sys.time(), "%F %T:"), message)),
    ## c("i" =  message),
    ...,
    class = "system_message",
    .envir = parent.frame()
  )
}

user_message <- function(message, ...) {
  if (isRunning()) {
    showNotification(message, type = "message")
  }
  cli_inform(
    c("i" = message),
    ...,
    class = "user_message",
    .envir = parent.frame()
  )
}

## #' @importFrom cli ansi_strip
## with_user_messages <- function(expr) {
##   if (isRunning()) {
##     withCallingHandlers(
##       expr,
##       user_message = function(m) {
##         b
##         showNotification(ansi_strip(m$message), type = "message")
##       }
##     )
##   } else {
##     expr
##   }
## }
