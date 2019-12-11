#' filter.webElement
#' @export
filter.webElement <- function(elements, attr, value){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  values <- elements %>%
    get_attribute(attr)
  
  to_select <- which(values %in% value)
  
  if(length(to_select) != 0){
    out <- elements[to_select]
    if(length(out) == 1){out <- out[[1]]}
    return(out)
  } else {
    return(list())
  }
  
}

#' screenshot
#' @export
screenshot <- function(browser, display = T, useViewer = T, file = NULL){
  browser$screenshot(display, useViewer, file)
}

#' wait_until
#' @export
wait_until <- function(chrome, n_wait = 4, value = "", using = "css selector", return = "", click = F){
  n <- n_wait*2
  while(n > 0){
    res <- silently(try(chrome %>% element(value = value, using = using), silent = T))
    n <- ifelse(class(res) == "try-error", n -.5, 0)
    Sys.sleep(.5)
  }
  if(click){chrome %>% click(value, using, return)}
}

#' wait_and_click
#' @export
wait_and_click <- function(chrome, n_wait = 4, value = "", using = "css selector", return = ""){
  wait_until(chrome, n_wait = n_wait, value = value, using = using, return = return, click = T)
}


