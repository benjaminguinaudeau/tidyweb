
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyweb <img src="man/figures/tidyweb_logo.png" width="160px" align="right" />

[![](https://img.shields.io/github/languages/code-size/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb)
[![](https://img.shields.io/github/last-commit/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb/commits/master)

This package is meant to ease web scraping with Selenium by “tidying”
the html structure. To do so, it iterates recursively on web elements
until a given depth and returns a tibble, with the children elements
nested in list-columns. That way, tidy principles can be used to
identify specific elements and eventually interact with them.

## Install

``` r
# remotes::install_github("benjaminguinaudeau/tidyweb")
devtools::load_all()
```

## How to use?

I like to use the [dockeR](http://github.com/benjaminguinaudeau/dockeR)
package to use a Selenium server running in a docker container, but this
will work with any driver generated from RSelenium. This only work if
docker is installed on your computer and the deamon running.

``` r
# remotes::install_github("benjaminguinaudeau/dockeR")
tidyweb::quiet(library(tidyverse))

chrome <- chrome_init("chrome", view = F, ua = NULL) # View param only works for mac
```

For this example, I will scrape the analysis provided by socialblade of
CNN’s youtube channel.

``` r
chrome %>% 
  open %>%
  go("https://socialblade.com/youtube/user/cnn")
```

``` r
body <- chrome %>%  elements("#socialblade-user-content")
body
#> [[1]]
#> [1] "remoteDriver fields"
#> $remoteServerAddr
#> [1] "localhost"
#> 
#> $port
#> [1] 32769
#> 
#> $browserName
#> [1] "chrome"
#> 
#> $version
#> [1] ""
#> 
#> $platform
#> [1] "ANY"
#> 
#> $javascript
#> [1] TRUE
#> 
#> $nativeEvents
#> [1] TRUE
#> 
#> $extraCapabilities
#> $extraCapabilities$chromeOptions
#> $extraCapabilities$chromeOptions$prefs
#> $extraCapabilities$chromeOptions$prefs$profile.default_content_settings.popups
#> [1] 0
#> 
#> 
#> $extraCapabilities$chromeOptions$args
#> [1] "--disable-dev-shm-usage" "--disable-gpu"          
#> 
#> 
#> 
#> [1] "webElement fields"
#> $elementId
#> [1] "9e791269-9348-4743-9c65-d2c2c69fde2a"
```

## Lets start by exploring the first depth-level

``` r
depth_one <- body %>% tidy_element(1) %>% glimpse
#> Current depth: 1
#> Observations: 1
#> Variables: 4
#> $ id         <chr> "socialblade-user-content"
#> $ element    <list> [<S4 class 'webElement' [package "RSelenium"] with 1…
#> $ id_parent  <int> 1
#> $ children_1 <list> [<tbl_df[32 x 13]>]
depth_one$children_1[[1]] %>% glimpse
#> Observations: 32
#> Variables: 13
#> $ style         <chr> "float: left; width: 900px; height: 150px;", "clea…
#> $ element       <list> [<S4 class 'webElement' [package "RSelenium"] wit…
#> $ src           <chr> NA, NA, NA, NA, "/js/Semaphore.js?version=9001.010…
#> $ charset       <chr> NA, NA, NA, NA, "utf-8", NA, NA, NA, NA, NA, NA, N…
#> $ rel           <chr> NA, NA, NA, NA, NA, "stylesheet", NA, NA, NA, NA, …
#> $ href          <chr> NA, NA, NA, NA, NA, "/css/Semaphore.css?version=90…
#> $ id            <chr> NA, NA, NA, NA, NA, NA, "Semaphore-Template", NA, …
#> $ country       <chr> NA, NA, NA, NA, NA, NA, "DE", NA, NA, NA, NA, NA, …
#> $ class         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ type          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ id_parent     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ id_children_1 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ depth         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
```

``` r
body <- chrome %>%  elements("#socialblade-user-content")
depth_three <- body %>% tidy_element(3) %>% glimpse
#> Current depth: 1
#> Current depth: 2
#> Current depth: 3
#> Observations: 1
#> Variables: 6
#> $ id         <chr> "socialblade-user-content"
#> $ element    <list> [<S4 class 'webElement' [package "RSelenium"] with 1…
#> $ id_parent  <int> 1
#> $ children_1 <list> [<tbl_df[32 x 13]>]
#> $ children_2 <list> [<tbl_df[101 x 11]>]
#> $ children_3 <list> [<tbl_df[108 x 15]>]
depth_three$children_3[[1]] %>% glimpse
#> Observations: 108
#> Variables: 15
#> $ style         <chr> "width: 122px; height: 80px; margin-top: 10px; lin…
#> $ element       <list> [<S4 class 'webElement' [package "RSelenium"] wit…
#> $ id_parent     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ id_children_1 <int> 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 7, 7, 7, 7,…
#> $ id_children_2 <int> 1, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 3, 3, 1, 2, 2, 2,…
#> $ id_children_3 <int> 1, 2, 1, 2, 3, 4, 5, 1, 2, 1, 2, 1, 2, 1, 1, 2, 3,…
#> $ depth         <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,…
#> $ class         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ hint          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ method        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ title         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ role          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ id            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ type          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ dir           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

``` r
depth_three %>%
  select(contains("children")) %>%
  map_dfr(1) %>%
  filter(style %>% str_detect("color:#41a200;"))
#> # A tibble: 7 x 22
#>   style element src   charset rel   href  id    country class type 
#>   <chr> <list>  <chr> <chr>   <chr> <chr> <chr> <chr>   <chr> <chr>
#> 1 font… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 2 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 3 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 4 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 5 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 6 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> 7 colo… <webEl… <NA>  <NA>    <NA>  <NA>  <NA>  <NA>    <NA>  <NA> 
#> # … with 12 more variables: id_parent <int>, id_children_1 <int>,
#> #   depth <dbl>, id_children_2 <int>, target <chr>, chart <chr>,
#> #   id_children_3 <int>, hint <chr>, method <chr>, title <chr>,
#> #   role <chr>, dir <chr>
```
