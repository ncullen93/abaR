
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

A meta-goal of the abaR framework is to cater to users at a wide range
of abstraction levels. This means that abaR should work equally well for
users whether they want a high-level framework to analyze biomarkers
without thinking much about the underlying statistics, or wether they
want a low-level framework where they just want a way to easily run
their own highly custom statistical models on different parameter
combinations and across cohorts.

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
  # set_data(ADNI_EXAMPLE)
  set_groups(
    #DX_bl == 'CU',
    #DX_bl == 'MCI',
    #DX_bl %in% c('CU', 'MCI')
  ) %>% 
  set_outcomes(
    #MMSE_4y:ADAS_4y
  ) %>% 
  set_covariates(
    #AGE, SEX, EDUCATION
  ) %>% 
  set_predictors(
    #ABETA, TAU, NFL,
    #c(ABETA, TAU, NFL)
  )
  
```
