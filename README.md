
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nottshcMethods

<!-- badges: start -->

[![R-CMD-check](https://github.com/CDU-data-science-team/nottshcMethods/workflows/R-CMD-check/badge.svg)](https://github.com/CDU-data-science-team/nottshcMethods/actions)
<!-- badges: end -->

The goal of nottshcMethods is to help us analyse our data faster and
better.

## Installation

You can install from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("CDU-data-science-team/nottshcMethods")
```

## Slide and distill post templates

Once the package is installed, {xaringan} presentation slides with
branding and {distill} post templates can be access easily using the
RStudio IDE, by selecting `File` -\> `R markdown ...` -\> `From
Template`.

<img src="img/screen-capture-templates.png" title="Screenshot of the templates wizard selection list with Nottshc Presentation Slides highlighted" alt="Screenshot of the templates wizard selection list with Nottshc Presentation Slides highlighted" width="50%" />

# Functions

## year\_date()

A simple function to give the ‘yyyy’ string to a date column (of formats
date, int and character). Can be used to find the earliest and latest
year date in data which is useful in reports.

<https://cdu-data-science-team.github.io/nottshcMethods/articles/date_helper.html>

## Code of Conduct

Please note that the nottshcMethods project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
