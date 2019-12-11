#' chrome_init
#' @export
chrome_init <- function(view = T, name = "", ua = 1){
  
  name <- ifelse(name == "", "chrome", name)
  
  if(!name %in% dockeR::existing_containers()){
    dockeR::create_container("selenium/standalone-chrome-debug", name)
    bashR::wait(4, .5)
  }
  if(name %in% dockeR::stopped_containers()){
    dockeR::start_container(name)
    bashR::wait(4, .5)
  }
  if(name %in% dockeR::running_containers()){
    chrome <- dockeR::quiet(dockeR::get_driver(port = dockeR::get_port(name, 4444), ua = ua))
  }
  
  if(view == T){dockeR::view_container(name)}
  return(chrome)
}

#' get_driver
#' @export
get_driver <- function(port, ua = 1){
  eCaps <- list(
    chromeOptions =
      list(
        prefs = list(
          "profile.default_content_settings.popups" = 0L
          # "download.prompt_for_download" = F
          # #"download.default_directory" = "~/extract_temp"
        ),
        args = c('--disable-dev-shm-usage',
                 '--disable-gpu',
                 ifelse(is.null(ua), "", glue::glue('--user-agent="{dockeR::user_agents[ua]}"')))# '--no-sandbox', '--headless') #  '--window-size=1200,1800' , ,
      )
  )
  
  driver <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = "chrome",
    extraCapabilities = eCaps
  )
  
  driver$open()
  
  return(driver)
}

#' renew_window
#' @export
renew_window <- function(chrome, max = T){
  if(!max){max_size <- function(x) return(x)}
  chrome %>%
    close_all %>% open %>% max_size
  return(invisible(chrome))
}


#' open.remoteDriver
#' @export
open.remoteDriver <- function(chrome){dockeR::quiet(chrome$open()) ; return(invisible(chrome))}

#' close_all
#' @export
close_all <- function(chrome){chrome$closeall() ; return(invisible(chrome))}

#' max_size
#' @export
max_size <- function(chrome){chrome$maxWindowSize(); return(invisible(chrome))}


#' screenshot
#' @export
screenshot <- function(browser, file = NULL, display = T, useViewer = T){
  browser$screenshot(file = file, display = display, useViewer = useViewer)
  return(invisible(browser))
}

#' new_window
#' @export
new_window <- function(port = 4444, prune = T, browser = "chrome"){
  tmp <- get_driver(remoteServerAddr = "selenium", port = as.integer(port), browserName = browser)
  if(prune) tmp$closeall()
  tmp$open()
  return(tmp)
}

#' get_source_code
#' @export
get_real_source_code <- function(browser, filepath = NULL){
  
  tmp <- browser$executeScript("return window.document.getElementsByTagName('html')[0].innerHTML")
  
  page <- tmp[[1]] %>%
    xml2::read_html(.)
  
  if(is.null(filepath)){
    return(page)
  } else {
    page %>% xml2::write_html(., file = filepath)
    message(glue::glue("Source code was saved under { filepath }"))
  }
  
}
