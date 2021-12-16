
test_that("errors in model fit are properly handled", {
  # create a confounded variable
  data <- adnimerge %>%
    dplyr::mutate(PTAU_NFL_RATIO_bl = PLASMA_PTAU181_bl + PLASMA_NFL_bl)

  # a few models failing should still works
  expect_error(
    suppressWarnings(
      model <- data %>% aba_model() %>%
        set_groups(DX_bl == 'MCI', everyone()) %>%
        set_outcomes(CDRSB) %>%
        set_predictors(
          PLASMA_PTAU181_bl,
          PLASMA_NFL_bl,
          PTAU_NFL_RATIO_bl,
          c(PLASMA_PTAU181_bl, PLASMA_NFL_bl, PTAU_NFL_RATIO_bl),
          labels = c('T', 'N', 'TN', 'TN2')
        ) %>%
        set_covariates(AGE, GENDER, EDUCATION) %>%
        set_stats(
          stat_lme(id='RID', time='YEARS_bl')
        ) %>%
        aba_fit()
      ),
    NA
  )

  expect_error(
    ms <- model %>% aba_summary(),
    NA
  )

  # all models failing should give an error
  expect_error(
    suppressWarnings(
    model <- data %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(CDRSB) %>%
      set_predictors(
        AGE, GENDER, EDUCATION
      ) %>%
      set_covariates(PLASMA_PTAU181_bl, PLASMA_NFL_bl, PTAU_NFL_RATIO_bl) %>%
      set_stats(
        stat_lme(id='RID', time='YEARS_bl')
      ) %>%
      aba_fit()
    ),
    'failed to be fit'
  )

})


test_that("standard lme works", {
  expect_error(
    model <- adnimerge %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(MMSE, CDRSB) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        stat_lme(id='RID', time='YEARS_bl')
      ) %>%
      aba_fit(),
    NA
  )

  expect_error(
    model <- adnimerge %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(MMSE, CDRSB) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        stat_lme(id='RID', time='YEARS_bl')
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )
})

test_that("forgetting parameters throws error", {
  model <- adnimerge %>% aba_model() %>%
    set_groups(DX_bl == 'MCI', everyone()) %>%
    set_outcomes(MMSE, CDRSB) %>%
    set_predictors(
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION)

  expect_error(
    model %>%
      set_stats(stat_lme(id='RID')) %>%
      aba_fit()
  )
  expect_error(
    model %>%
      set_stats(stat_lme(time='YEARS_bl')) %>%
      aba_fit()
  )
  expect_error(
    model %>%
      set_stats(stat_lme()) %>%
      aba_fit()
  )

})
