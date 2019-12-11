
#' tidy_element
#' @param elems a list of selenium-elements 
#' @param depth a numeric value indicating how deep the search should go.
#' @return A tibble with the elements attribute and a column for each layer containing the children from the corresponding layer.
#' @export

tidy_element <- function(elems, depth = 0){
  
  out <- elems %>%
    purrr::map_dfr(tidyweb::get_all_attribute) %>%
    dplyr::mutate(id_parent = 1:n())
  
  depth_index <- 0
  
  while(depth_index != depth){
    
    depth_index <- depth_index + 1
    message("Current depth: ", depth_index)
    child_col <- paste0("children_", depth_index)
    child_id_col <- paste0("id_children_", depth_index)
    element_col <- paste0("element_", "depth_index")
    
    if(depth_index != 1){
      parent_col <- dplyr::sym(paste0("children_", depth_index - 1))
      if(all(out %>% dplyr::pull({{parent_col}}) %>% purrr::map_lgl(~length(.x) == 0))){
        message("Max depth was reached")
        # out <- out %>% select(-parent_col)
        return(out)
      }
    }
    
    if(depth_index == 1){
      
      out <- out %>%
        dplyr::mutate({{child_col}} := purrr::map2(element, id_parent,~{
          .x %>%
            tidyweb::find_children("*", "xpath") %>%
            purrr::map_dfr(tidyweb::get_all_attribute) %>%
            dplyr::mutate(id_parent = .y,
                   {{child_id_col}} := 1:n(),
                   depth = depth_index)
        }))
    } else {
      
      out <- out %>%
        mutate(parents = {{parent_col}}) %>%
        mutate({{child_col}} := map(parents, ~{
          if(length(.x) != 0){
            parent <- .x
            # child <- children[[1]]
            
            parent %>%
              split(1:nrow(.)) %>%
              imap_dfr(~{
                children <- .x$element[[1]] %>%
                  find_children("*", "xpath")
                
                if(length(children) > 0){
                  children_attr <- children %>%
                    purrr::map_dfr(tidyweb::get_all_attribute) %>%
                    cbind(dplyr::select(.x, dplyr::contains("id_"))) %>%
                    dplyr::as_tibble %>%
                    dplyr::mutate({{child_id_col}} := 1:n(),
                           depth = depth_index)
                }
              })
          }
        })) %>%
        dplyr::select(-parents)
    }
  }
  return(out)
}

