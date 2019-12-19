
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyweb <img src="man/figures/tidyweb_logo.png" width="160px" align="right" />

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://img.shields.io/github/languages/code-size/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb)
[![](https://img.shields.io/github/last-commit/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb/commits/master)

> Use tidy data principles to interact with HTML files\!

This package is meant to ease web scraping with Selenium by “tidying”
the html structure. To do so, it iterates recursively on web elements
until a given depth and returns a tibble, with the children elements
nested in list-columns. That way, tidy principles can be used to
identify specific elements and eventually interact with them.

## Install

``` r
# remotes::install_github("benjaminguinaudeau/tidyweb")
library(tidyweb)
library(dplyr)
```

## How to use with Rvest?

``` r
page <- xml2::read_html("https://www.nytimes.com/")

art <- page %>%
  rvest::html_nodes("article")


parsed_art <- art %>% tidy_element(depth = 10) 

parsed_art %>% glimpse
parsed_art %>% filter(!is.na(href)) %>% glimpse
parsed_art %>% 
  separate_rows(class, sep = "\\s+") %>%
  count(class, sort = T) %>%
  glimpse

parsed_art %>% 
  mutate(depth = str_count(.id, "_") + 1) %>%
  group_by(depth) %>%
  ggplot(aes(x = depth)) + geom_histogram()
  
```

## Thanks

A huge thank you to [Favstats](https://github.com/favstats) for
designing each of the hex-stickers.
