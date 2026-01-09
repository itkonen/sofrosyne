## cache <- function(x, ..., refresh = FALSE) {
##   key <- rlang::hash(list(...))
##   .cache <- getOption("shiny.cache")
##   if (refresh) {
##     .cache$remove(key)
##   }
##   y <- .cache$get(key)
##   if (refresh || is.key_missing(y)) {
##     x <- force(x)
##     .cache$set(key, x)
##     x
##   } else {
##     y
##   }
## }


#' @importFrom rlang abort try_fetch caller_env
with_errors <- function(expr, message, call = caller_env()) {
  try_fetch(
    expr,
    error = function(cnd) {
      abort(message, parent = cnd, call = call)
    }
  )
}

#' Load one-by-one
#'
#' Tag a reactive output to postpone all similarly tagged outputs to the next flush
one_per_flush <- function() {
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    busy <- isolate(session$userData$.busy)
    if (is.null(busy) || !busy) {
      session$userData$.busy <- TRUE
      session$onFlushed(function() {
        session <- getDefaultReactiveDomain()
        session$userData$.busy <- FALSE
      })
    } else {
      invalidateLater(1, session)
      req(FALSE)
    }
  }
}
