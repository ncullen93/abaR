
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated biomarker analysis in R (abaR)

<!-- badges: start -->
<!-- badges: end -->

The goal of abaR is to make it easier to fit statistical models on
permuatations of covariates / predictors / outcomes / groups, to make it
easier to investigate how biomarkers can be used to plan (screening /
enirichment) and analyze clinical trials, and to facilitate multi-cohort
validation studies when data cannot be easily shared between
collaborators.

The abaR package is based on tidyverse principles, which is why it
features a workflow heavily based on the pipe ( %&gt;% ) operator. This
lets you analyze data in a manner which is more similar to the way we
talk and think about biomarker analysis.

## Installation

You can install the development version from
[GitHub](https://github.com/ncullen93/abaR) with:

``` r
# install.packages("devtools")
devtools::install_github("ncullen93/abaR")
```

## Example: statistical analysis

The general workflow of fitting an aba model for statistical analysis
looks like this:

``` r
model <- aba_model() %>% 
  set_groups() %>% 
  set_outcomes() %>% 
  set_covariates() %>% 
  set_predictors()
  
```
