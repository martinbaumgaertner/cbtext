
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

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("martinbaumgaertner/cbtext")
```

## Available central banks and Types

| Central Bank/ Institution         |             | Type                                                                                                                |
|-----------------------------------|-------------|---------------------------------------------------------------------------------------------------------------------|
| Bank of International Settlements | `bis`       | `speeches`                                                                                                          |
| European Central Bank             | `ecb`       | `minutes` `pc` `economic_outlook` `interview` `blog`                                                                |
| US Federal Reserve                | `fed`       | `minutes` `pc` `statement` `beige` `green1` `green2` `green_sub` `teala` `tealb` `blue` `agenda` `red` `transkript` |
| Bank of Japan                     | `boj`       | `minutes` `economic_report release` `outlook_report`                                                                |
| Sveriges Riksbank                 | `riksbank`  | `minutes` `mpreports` `economicreview`                                                                              |
| Bank of England                   | `boe`       | `minutes`                                                                                                           |
| Narodowy Bank Polski              | `poland`    | `minutes`                                                                                                           |
| Central Bank of Iceland           | `iceland`   | `minutes`                                                                                                           |
| Reserve Bank of Australia         | `australia` | `minutes`                                                                                                           |

All central bank texts and were selected according to the fact that they
were easy to find via HTML. Another criterion is that the text type is
clearly delineated within the category.

Very much I would like to extend the tool’s range. So if you are missing
a central bank or a text type, please send me a message and we will see
if this is possible!

## Example

``` {library(cbtext)}
x<-scrape_source(list(name="ecb"),c("minutes"))
x
```
