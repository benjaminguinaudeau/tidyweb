#' tidy_element
#' @param element a list of xml nodes representing individual divs or a list of selenium-elements 
#' @param depth a numeric value indicating how deep the search should go.
#' @return A tibble with the elements attribute and a column for each layer containing the children from the corresponding layer.
#' @export

tidy_element <- function(element, depth = 0, quiet = F){
  
  if(!class(element)[1] %in% c("xml_nodeset", "list")) element <- list(element)
  
  if(class(element[[1]])[1] == "xml_node"){
    get_children <- xml2::xml_children
    get_attr <- get_attributes
  } else {
    get_children <- function(x) find_children(x, "*", "xpath")
    get_attr <- tidyselenium::get_all_attributes
  }
  
  
  
  out <- element %>% 
    purrr::map_dfr(get_attr, text = T) %>%
    dplyr::mutate(id_parent = 1:n())
  
  depth_index <- 0
  
  while(depth_index != depth){
    
    parent_col <- dplyr::sym(paste0("children_", depth_index))
    
    if(depth_index != 0){
      if(all(out[[parent_col]] %>% purrr::map_lgl(~length(.x) == 0))){
        if(!quiet){message("Max depth was reached")}
        break(out)
      }
    }
    
    depth_index <- depth_index + 1
    if(!quiet){
      utils::flush.console()
      cat("\rCurrent depth: ", depth_index)
    }
    
    child_col <- paste0("children_", depth_index)
    child_id_col <- paste0("id_children_", depth_index)
    
    
    if(depth_index == 1){
      
      out <- out %>% 
        dplyr::mutate({{child_col}} := purrr::map2(element, id_parent,~{
          child <- .x %>%
            get_children 
          
          if(length(child) != 0){
            out <- child %>% 
              purrr::map_dfr(get_attr, text = T)  %>%
              dplyr::mutate(id_parent = .y,
                            {{child_id_col}} := 1:dplyr::n(),
                            depth = {{depth_index}})
          } 
          return(out)
        }))
      
    } else {
      out <-  out %>%
        dplyr::mutate(parents = {{parent_col}}, 
                      {{child_col}} := purrr::map(parents, ~{
                        
                        if(length(.x) != 0){
                          
                          .x %>%
                            split(1:nrow(.)) %>%
                            purrr::imap_dfr(~{
                              children <-  .x$element[[1]] %>%
                                get_children
                              
                              if(length(children) > 0){
                                children_attr <-   children %>%
                                  purrr::map_dfr(get_attr, text = T) %>%
                                  dplyr::filter(!is.null(element)) %>%
                                  cbind(dplyr::select(.x, dplyr::contains("id_"))) %>%
                                  dplyr::as_tibble() %>%
                                  dplyr::mutate({{child_id_col}} := 1:dplyr::n(),
                                                depth = {{depth_index}})
                                
                                return(children_attr)
                              }
                              
                            })
                        }
                      })) %>%
        dplyr::select(-parents)
    }
    
    
  }
  
  out <- out %>%
    dplyr::select(dplyr::contains("children")) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::bind_rows(dplyr::select(out, -dplyr::contains("children_")), .) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("id_children"), id_parent), as.character) %>%
    split(1:nrow(.)) %>%
    purrr::map_dfr(get_unique_id) %>%
    dplyr::select(.id, element, text, dplyr::everything())
  
  return(out)
}


# tidy_element <- function(element, depth = 0){
#   
#   out <- element %>% get_attributes # to adapt
#   
#   depth_index <- 0
#   
#   while(depth_index != depth){
#     
#     depth_index <- depth_index + 1
#     message("Current depth: ", depth_index)
#     child_col <- paste0("children_", depth_index)
#     child_id_col <- paste0("id_children_", depth_index)
#     element_col <- paste0("element_", "depth_index")
#     
#     if(depth_index != 1){
#       parent_col <- dplyr::sym(paste0("children_", depth_index - 1))
#       if(all(out %>% dplyr::pull({{parent_col}}) %>% purrr::map_lgl(~length(.x) == 0))){
#         message("Max depth was reached")
#         break
#       }
#     }
#     
#     if(depth_index == 1){
#       # To adapt
#       out <- out %>% 
#         dplyr::mutate({{child_col}} := purrr::map2(element, id_parent,~{
#           .x %>%
#             xml2::xml_children() %>%
#             purrr::map_dfr(get_attributes) %>%
#             dplyr::mutate(id_parent = .y, {{child_id_col}} := 1:n(),
#                    depth = depth_index)
#         }))
#     } else {
#       
#       # To adapt
#       out <- out %>%
#         dplyr::mutate(parents = {{parent_col}}) %>%
#         dplyr::mutate({{child_col}} := purrr::map(parents, ~{
#           if(length(.x) != 0){
#             parent <- .x
#             
#             parent %>%
#               split(1:nrow(.)) %>%
#               purrr::imap_dfr(~{
#                 children <- purrr::reduce(.x$element, c) %>%
#                   rvest::html_children()
#                 
#                 if(length(children) > 0){
#                   children_attr <- children %>%
#                     purrr::map_dfr(get_attributes) %>%
#                     dplyr::filter(!is.null(element)) %>%
#                     dplyr::select(-id_parent) %>%
#                     cbind(dplyr::select(.x, dplyr::contains("id_"))) %>%
#                     dplyr::as_tibble() %>%
#                     dplyr::mutate({{child_id_col}} := 1:dplyr::n(),
#                            depth = depth_index)
#                 }
#               })
#           }
#         })) %>%
#         dplyr::select(-parents)
#     }
#   }
#   out <-   out %>%
#     dplyr::select(dplyr::contains("children")) %>%
#     purrr::reduce(dplyr::bind_rows) %>%
#     dplyr::bind_rows(dplyr::select(out, -dplyr::contains("children_"))) %>%
#     # select(id, class, id_parent, contains("id_children"), everything()) %>%
#     # glimpse
#     dplyr::mutate_at(dplyr::vars(dplyr::contains("id_children"), id_parent), as.character) %>%
#     split(1:nrow(.)) %>%
#     purrr::map_dfr(get_unique_id) %>%
#     dplyr::mutate(text = purrr::map_chr(element, ~{
#       .x %>%
#         rvest::html_text() %>%
#         stringr::str_squish()
#     })) %>%
#     dplyr::select(.id, element, text, dplyr::everything())
#   
#   return(out)
# }

# 
# 
# tidy_element_sel <- function(elems, depth = 0){
#   
#   out <- elems %>%
#     purrr::map_dfr(get_all_attribute) %>%
#     dplyr::mutate(id_parent = 1:n())
#   
#   depth_index <- 0
#   
#   while(depth_index != depth){
#     
#     depth_index <- depth_index + 1
#     message("Current depth: ", depth_index)
#     child_col <- paste0("children_", depth_index)
#     child_id_col <- paste0("id_children_", depth_index)
#     element_col <- paste0("element_", "depth_index")
#     
#     if(depth_index != 1){
#       parent_col <- dplyr::sym(paste0("children_", depth_index - 1))
#       if(all(out %>% dplyr::pull({{parent_col}}) %>% purrr::map_lgl(~length(.x) == 0))){
#         message("Max depth was reached")
#         # out <- out %>% select(-parent_col)
#         return(out)
#       }
#     }
#     
#     if(depth_index == 1){
#       
#       out <- out %>%
#         dplyr::mutate({{child_col}} := purrr::map2(element, id_parent,~{
#           .x %>%
#             find_children("*", "xpath") %>%
#             purrr::map_dfr(get_all_attribute) %>%
#             dplyr::mutate(id_parent = .y,
#                    {{child_id_col}} := 1:dplyr::n(),
#                    depth = depth_index)
#         }))
#     } else {
#       
#       out <- out %>%
#         dplyr::mutate(parents = {{parent_col}}) %>%
#         dplyr::mutate({{child_col}} := purrr::map(parents, ~{
#           if(length(.x) != 0){
#             parent <- .x
#             # child <- children[[1]]
#             
#             parent %>%
#               split(1:nrow(.)) %>%
#               purrr::imap_dfr(~{
#                 children <- .x$element[[1]] %>%
#                   find_children("*", "xpath")
#                 
#                 if(length(children) > 0){
#                   children_attr <- children %>%
#                     purrr::map_dfr(get_all_attribute) %>%
#                     cbind(dplyr::select(.x, dplyr::contains("id_"))) %>%
#                     dplyr::as_tibble() %>%
#                     dplyr::mutate({{child_id_col}} := 1:dplyr::n(),
#                            depth = depth_index)
#                 }
#               })
#           }
#         })) %>%
#         dplyr::select(-parents) 
#     }
#   }
#   
#   out <-   out %>%
#     dplyr::select(dplyr::contains("children")) %>%
#     purrr::reduce(dplyr::bind_rows) %>%
#     dplyr::bind_rows(dplyr::select(out, -dplyr::contains("children_"))) %>%
#     # select(id, class, id_parent, contains("id_children"), everything()) %>%
#     # glimpse
#     dplyr::mutate_at(dplyr::vars(dplyr::contains("id_children"), id_parent), as.character) %>%
#     split(1:nrow(.)) %>%
#     purrr::map_dfr(get_unique_id) %>%
#     dplyr::mutate(text = purrr::map_chr(element, ~{
#       .x %>%
#         tidyselenium::get_text() %>%
#         stringr::str_squish()
#     })) %>%
#     dplyr::select(.id, element, text, dplyr::everything())
#   
#   return(out)
#   
# }
# 
