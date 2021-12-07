test_that("standard mmrm works", {

  # fit
  expect_error(
    model <- adnimerge %>%
      dplyr::filter(
        VISCODE %in% c('bl', 'm06', 'm12', 'm24')
      ) %>%
      aba_model() %>%
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
        aba_mmrm(id='RID', time='VISCODE')
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )


})
