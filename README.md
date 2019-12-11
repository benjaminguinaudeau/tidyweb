
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyweb <img src="man/figures/tidyweb_logo.png" width="160px" align="right" />

[![](https://img.shields.io/github/languages/code-size/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb)
[![](https://img.shields.io/github/last-commit/benjaminguinaudeau/tidyweb.svg)](https://github.com/benjaminguinaudeau/tidyweb/commits/master)

This package is meant to ease web scraping with Selenium by “tidying”
the html structure. To do so, it iterates recursively on web elements
until a given depth and returns a tibble, with the children elements
nested in list-columns. That way, tidy principles can be used to
identify specific elements and eventually interact with them.
