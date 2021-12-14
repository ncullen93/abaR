test_that("example works", {
  # use built-in data
  df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # first, fit an aba model to predict amyloid PET status from plasma markers
  # In this scenario, PET is the "inclusion" marker and plasma is the
  # "screening" marker. PET is expensive and plasma is cheap, so we want to
  # use plasma markers to decide who should undergo PET scans in order to
  # minimize the risk of negative (i.e., wasted) PET scans.
  model <- df %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(PET_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('glm') %>%
    fit()
  # summarise the model just to show the plasma biomarkers do in fact
  # provide some predictive value for amyloid PET status
  model_summary <- model %>% aba_summary()
  # Run the screening analysis while varying the inclusion threshold from
  # 25% to 75% (this is the percent of individuals who will be invited for
  # the PET scan) and varying the cost multiplier from 4 to 16 (this is how
  # much more PET costs compared to plasma) and assuming we want to recruit
  # 1000 amyloid PET positive subjects.
  expect_error(
    model_screen <- model %>%
      aba_screen(
        threshold = seq(0.25, 0.75, by = 0.25),
        cost_multiplier = c(4, 8),
        include_n = 1000,
        ntrials = 3,
        verbose = TRUE
      ),
    NA
  )
})
