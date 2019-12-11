#' go
#' @export
go <- function(browser, ...){
  browser$navigate(...)
  return(invisible(browser))
}

#' element
#' @export
element <- function(browser,  value, using = "css selector"){
  browser$findElement(using, value)
}

#' elements
#' @export
elements <- function(browser,  value, using = "css selector"){
  browser$findElements(using, value)
}

#' find_child
#' @export
find_child <- function(element, value = "", using = "css selector"){element$findChildElement(value = value, using = using)}
#' find_children
#' @export
find_children <- function(element, value = "", using = "css selector"){element$findChildElements(value = value, using = using)}

#' switch_to_window
#' @export
switch_to_window <- function (chrome, window_handle){
  qpath <- sprintf("%s/session/%s/window", chrome$serverURL, chrome$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
  
  return(invisible(chrome))
}

#' switch_to_frame
#' @export
switch_to_frame <- function(chrome, div_value = "", div_using = "css selector",
                            frame_value = "", frame_using = "name"){
  elem <- chrome %>%
    element(div_value, div_using) %>%
    find_child(frame_value, frame_using)
  
  chrome$switchToFrame(elem)
  return(invisible(chrome))
}


#' set_attribute
#' @export
set_attribute <- function(elements, attr, value){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  elements %>%
    purrr::map_chr(~{
      out <- .x$setElementAttribute(attributeName = attr, value = value)
      out <- ifelse(length(out) == 0, NA_character_, out[[1]])
      return(out)
    })
}

#' get_attribute
#' @export
get_attribute <- function(elements, attr){
  if(class(elements)[[1]] != "list"){elements <- list(elements)}
  purrr::map_chr(elements, ~{
    out <- .x$getElementAttribute(attr)
    out <- ifelse(length(out) == 0, NA_character_, out[[1]])
    return(out)
  })
}

#' get_class
#' @export
get_class <- function(elems){
  elems %>% get_attribute("class")
}


#' get_text
#' @export
get_text <- function(element){element$getElementText()[[1]]}

#' get_all_attribute
#' @export

get_all_attribute <- function(element, text = F){
  out <- element %>%
    tidyweb::get_attribute("outerHTML") %>%
    stringr::str_extract("<.*?>") %>%
    stringr::str_extract_all('\\w+=\\".*?\\"') %>% .[[1]] %>%
    stringr::str_split("\\=", n = 2) %>%
    purrr::map_dfc(~{
      tibble::tibble(stringr::str_remove_all(.x[2], '"')) %>%
        purrr::set_names(.x[1])
    }) %>%
    dplyr::mutate(element = list(element))
  
  if(nrow(out) == 0){
    out <- tibble(element = list(element))
  }
  
  if(text){
    out <- out %>% mutate(text = element %>% map_chr(get_text))
  }
  return(out)
}



#' send_keys
#' @export
send_keys <- function(browser, keys = list(), return = "browser"){
  browser$sendKeysToElement(keys)
  if(return == "element") return(invisible(elem))
  if(return == "browser") return(invisible(browser))
}

#' keys
#' @export

keys <- RSelenium::selKeys

#' clear
#' @export

clear <- function(element){
  element$clearElement()
  return(invisible(element))
}

#' click
#' @export
click <- function(browser, value, using = "css selector", return = "browser"){
  if("remoteDriver" %in% class(browser)){
    elem <- browser$findElement(using, value)
    elem$clickElement()
    if(return == "element") return(elem)
    if(return == "browser") return(browser)
  } else {
    browser$clickElement()
  }
  Sys.sleep(sample(1:500, 1)/1000)
}

#' highlight
#' @export
highlight <- function(element, wait = .1){
  element$highlightElement(wait = wait)
  return(invisible(element))
}




#' check_element
#' @export
check_element <- function(chrome, value, using = "css selector"){
  element <- silently(try(chrome$findElement(using, value), silent = T))
  if(class(element)[1] == "try-error"){
    return(F)
  } else {
    return(T)
  }
}



