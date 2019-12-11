#' quiet
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' silently
#' @export
silently <- function(x){
  suppressMessages(suppressWarnings(x))
}
