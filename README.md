---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# demprep

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/bayesiandemography/demprep.svg?branch=master)](https://travis-ci.com/bayesiandemography/demprep)
[![Lifecycle status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**demprep** contains general-purpose functions to preparing demographic data for further analysis.


## Installation

You can install from GitHub with:

``` r
devtools::install_github("demprep")
```

## Usage

```r
library(demprep)
date_to_age_group_year(date = "2019-03-07",
                       dob = "2001-02-18")
make_labels_period(breaks = c("2001-07-01",
                              "2006-07-01",
			      "2011-07-01"))
```



