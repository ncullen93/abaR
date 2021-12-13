
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated biomarker analysis in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/ncullen93/abaR/workflows/R-CMD-check/badge.svg)](https://github.com/ncullen93/abaR/actions)
[![Codecov test
coverage](https://codecov.io/gh/ncullen93/abaR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ncullen93/abaR?branch=main)
<!-- badges: end -->

The goals of the aba package are the following:

-   to reduce effort and error associated with fitting statistical
    models across multiple analysis factors (groups, outcomes,
    predictors)
-   to allow anyone to generate publication-ready figures and tables
-   to provide advanced tools for biomarker-based planning and analysis
    of clinical trials
-   to facilitate multi-cohort validation studies

![overview image](inst/aba_overview.png)

With the abaR package, it becomes easier to fit statistical models on
permuatations of covariates / predictors / outcomes / groups, to make it
easier to investigate how biomarkers can be used to plan (screening /
enirichment) and analyze clinical trials, and to facilitate multi-cohort
validation studies when data cannot be easily shared between
collaborators.

## Get started

You can install the development version of the aba package from
[GitHub](https://github.com/ncullen93/abaR) with:

``` r
# install.packages("devtools")
devtools::install_github("ncullen93/abaR")
```

To view tutorials and examples of how to use the package, you can visit
the package website at <https://ncullen93.github.io/abaR>

Some examples of publications whose analysis have been completed mostly
or entirely with the aba package are feature here:

## A simple example

The general workflow of fitting an aba model for statistical analysis
looks like this:

``` r
library(aba)

df <- aba::adnimerge %>% filter(VISCODE == 'bl')

model_spec <- df %>% aba_model() %>%
  set_groups(everyone()) %>%
  set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
  set_predictors(
    PLASMA_ABETA_bl,
    PLASMA_PTAU181_bl,
    PLASMA_NFL_bl,
    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
  ) %>%
  set_stats(
    stat_glm(std.beta=T)
  )

model_fit <- model_spec %>% fit()
model_summary <- model_fit %>% summary()
```
