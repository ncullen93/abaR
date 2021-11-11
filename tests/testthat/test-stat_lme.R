test_that("standard lme works", {
  expect_error(
    model <- adni_sample %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(MMSE, CDRSB) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE_bl, GENDER, EDUCAT) %>%
      set_stats(
        aba_lme(id='SUBJECT_ID', time='Years_bl')
      ) %>%
      fit(),
    NA
  )
})

test_that("forgetting parameters throws error", {
  model <- adni_sample %>% aba_model() %>%
    set_groups(DX_bl == 'MCI', everyone()) %>%
    set_outcomes(MMSE, CDRSB) %>%
    set_predictors(
      PLASMA_ABETA_bl,
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE_bl, GENDER, EDUCAT)

  expect_error(
    model %>%
      set_stats(aba_lme(id='SUBJECT_ID')) %>%
      fit()
  )
  expect_error(
    model %>%
      set_stats(aba_lme(time='Years_bl')) %>%
      fit()
  )
  expect_error(
    model %>%
      set_stats(aba_lme()) %>%
      fit()
  )

})
