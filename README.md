# TopicLongTable

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/billdenney/TopicLongTableR/branch/master/graph/badge.svg)](https://codecov.io/gh/billdenney/TopicLongTableR?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/TopicLongTable)](https://CRAN.R-project.org/package=TopicLongTable)
[![R-CMD-check](https://github.com/billdenney/TopicLongTableR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/TopicLongTableR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of TopicLongTable is to enable generating tables which automatically group values together in row-spanning cells including awareness of new pages.

## Installation

You can install the released version of TopicLongTable from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("TopicLongTable")
```

## Example

This is a basic example which shows you how to generate a first table:

``` r
library(TopicLongTable)
TopicLongTable::topic_long_table(
  data.frame(A=1, B=rep(1:3, each=2), C=1:6),
  topic_cols=2,
  caption_args=list(text="This is a first table")
)
```
