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

#' push_github

push_github <- function(message = ""){
  system(glue::glue("git add -A && git commit -m {message} && git push"))
}


