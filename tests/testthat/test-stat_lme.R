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
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
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
      PLASMA_ABETA_bl,
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
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
