test_that("standard mmrm works", {

  # fit
  expect_error(
    model <- adni_sample %>% dplyr::filter(VISIT %in% c(0,1,2,3)) %>%
      aba_model() %>%
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
        aba_mmrm(id='SUBJECT_ID', time='VISIT')
      ) %>%
      fit() %>%
      aba_summary(),
    NA
  )


})
