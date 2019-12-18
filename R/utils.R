#' quiet
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' silently
silently <- function(x){
  suppressMessages(suppressWarnings(x))
}

#' push_github
push_github <- function(message = ""){
  system(glue::glue("git add -A && git commit -m {message} && git push"))
}

#' innerHTML
innerHTML <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}

#' outerHTML
outerHTML <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, "*"), collapse = collapse)
}

#' get_attributes
#' @export
get_attributes <- function(element, text = F){
  tmp <- element %>%
    as.character %>%
    stringr::str_extract("<.*?>") %>%
    stringr::str_extract_all('\\w+=\\".*?\\"') %>% .[[1]] %>%
    stringr::str_split("\\=", n = 2) %>%
    purrr::map_dfc(~{
      tibble::tibble(stringr::str_remove_all(.x[2], '"')) %>%
        purrr::set_names(.x[1]) 
    }) %>%
    dplyr::mutate(element = list(element))
  
  if(nrow(tmp) == 0){tmp <- tibble::tibble(element = list(element))}
  
  out <- tmp 
  
  if(text){
    out <- out %>% dplyr::mutate(text = element %>% purrr::map_chr(rvest::html_text))
  }
  return(out)
}

#' get_unique_id
#' @export
get_unique_id <- function(d){
  max <- d$depth
  
  if(is.na(max)){
    d %>%
      dplyr::mutate(.id = id_parent) %>%
      dplyr::select(-dplyr::contains("id_"))
  } else {
    
    id_col <- paste0("id_children_", max)
    to_use <- paste0("id_children_", 1:max)
    
    id <- d[,to_use] %>%
      t %>%
      paste0(collapse = "_") %>%
      paste(d$id_parent, .,  sep = "_")
    
    d %>%
      select(-contains("id_"), -depth) %>%
      mutate(.id = {{id}})
  }
}