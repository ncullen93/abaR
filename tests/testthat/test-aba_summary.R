test_that("standard summary works", {
  expect_error(
    m <- adnimerge %>% aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        everyone()
      ) %>%
      set_outcomes(
        ConvertedToAlzheimers,
        ConvertedToDementia
      ) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        'glm'
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )
})
