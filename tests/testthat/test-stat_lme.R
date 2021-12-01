test_that("standard lme works", {
  expect_error(
    model <- adnimerge %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(MMSE, CDRSB) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        aba_lme(id='RID', time='YEARS_bl')
      ) %>%
      fit(),
    NA
  )

  expect_error(
    model <- adnimerge %>% aba_model() %>%
      set_groups(DX_bl == 'MCI', everyone()) %>%
      set_outcomes(MMSE, CDRSB) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        aba_lme(id='RID', time='YEARS_bl')
      ) %>%
      fit() %>%
      aba_summary(),
    NA
  )
})

test_that("forgetting parameters throws error", {
  model <- adnimerge %>% aba_model() %>%
    set_groups(DX_bl == 'MCI', everyone()) %>%
    set_outcomes(MMSE, CDRSB) %>%
    set_predictors(
      PLASMA_ABETA_bl,
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION)

  expect_error(
    model %>%
      set_stats(aba_lme(id='RID')) %>%
      fit()
  )
  expect_error(
    model %>%
      set_stats(aba_lme(time='YEARS_bl')) %>%
      fit()
  )
  expect_error(
    model %>%
      set_stats(aba_lme()) %>%
      fit()
  )

})
