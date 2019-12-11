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