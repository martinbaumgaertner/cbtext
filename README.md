
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbtext

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/martinbaumgaertner/cbtext.svg?branch=master)](https://travis-ci.com/martinbaumgaertner/cbtext)
[![R-CMD-check](https://github.com/martinbaumgaertner/cbtext/workflows/R-CMD-check/badge.svg)](https://github.com/martinbaumgaertner/cbtext/actions)
[![Codecov test
coverage](https://codecov.io/gh/martinbaumgaertner/cbtext/branch/master/graph/badge.svg)](https://codecov.io/gh/martinbaumgaertner/cbtext?branch=master)
<!-- badges: end -->

The goal of cbtext is to provide an easy way to collect and read central
bank texts in R

## Installation

You can install the released version of cbtext from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cbtext")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("martinbaumgaertner/cbtext")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cbtext)
## basic example code
```

With this package you can scrape several central bank texts

``` r
scrape_source(list(name="ecb"),c("minutes"))
#> Find minutes
#> $minutes
#> # A tibble: 48 x 13
#>    title text                    speaker cb    country speaker_position location
#>    <lgl> <chr>                   <lgl>   <chr> <lgl>   <lgl>            <lgl>   
#>  1 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  2 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  3 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  4 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  5 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  6 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  7 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  8 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#>  9 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#> 10 NA    "Account of the moneta~ NA      ecb   NA      NA               NA      
#> # ... with 38 more rows, and 6 more variables: date <dttm>, release_date <lgl>,
#> #   type <chr>, chapter <lgl>, language <chr>, link <chr>
```
