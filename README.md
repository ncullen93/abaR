
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated biomarker analysis in R (abaR)

<!-- badges: start -->
<!-- badges: end -->

The goals of abaR are the following:

-   to make it easy to fit many statistical models
-   to provide high-level functions for common model visualizations
-   to improve workflows for biomarker-based planning and analysis of
    clinical trials
-   to facilitate multi-cohort validation studies without direct data
    sharing

With the abaR package, it becomes easier to fit statistical models on
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

## Statistical analysis

The general workflow of fitting an aba model for statistical analysis
looks like this:

``` r
library(aba)

model <- adni_sample %>% aba_model() %>%
  set_groups(everyone(), DX_bl == 'MCI') %>%
  set_outcomes(CSF_ABETA_STATUS_bl, ConvertedToAlzheimers) %>%
  set_predictors(
    PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl),
    c(PLASMA_ABETA_bl, PLASMA_NFL_bl),
    c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
  ) %>%
  set_covariates(AGE_bl, GENDER, EDUCAT) %>%
  set_stats('glm')

model <- model %>% fit()
model_summary <- model %>% aba_summary()
```

## Clinical trial statistical analysis

ABA can be used to analyze clinical trials using MMRM, linear
mixed-effects modelling, ANCOVA, and more. You can analyze multiple
endpoints and perform supplementary or sub-group analyses with one
smooth workflow.

The process for analyzing clinical trial data looks like this:

``` r
#model <- adni_sample %>% aba_trial() %>%
#  set_groups(everyone()) %>%
#  set_endpoints('NFL_change', 'CDRSB_change', 'ADL_change') %>%
#  set_treatment('TREATMENT') %>%
#  set_covariates(
#    'AGE', 'SEX', 'EDU', 'MMSE_bl'
#  ) %>%
#  set_stats(
#    aba_mmrm(id = 'SUBJID', time = 'WEEK'),
#    aba_lme(id = 'SUBJID', time = 'YEAR'),
#    aba_ancova(id = 'SUBJID', time = 'WEEK')
#  )
#
#model <- model %>% fit()
```

## Power analysis and biomarker-based trial planning

ABA can be used to investigate how biomarkers can be used to inform a
clinical trial screening / recruitment strategy. With ABA, you can
leverage existing cohort or trial data to understand the statistical
power associated with a given biomarker-based inclusion strategy.

The general workflow for fitting an aba trial for planning clinical
trials with the help of biomarker-based screening looks like this:

``` r
#model <- adni_sample %>% aba_trial() %>%
#  set_groups(
#    list(DX_bl == 'MCI', AGE_bl > 55, AGE_bl < 85),
#    DX_bl == 'MCI'
#  ) %>%
#  set_outcomes(
#    MMSE, CDRSB
#  ) %>%
#  set_times(
#    VISIT == 1.5,
#    VISIT == 2.0
#  ) %>%
#  set_stats(
#    stat_power(alpha=0.05, power=0.8, delta=0.3, method='t.test')
#  )
```
