test_that("example works", {
  model <- aba_model() %>%
    set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('glm') %>%
    aba_fit()

  expect_error(
    my_table <- model %>% aba_demographics(strata = 'DX_bl'),
    NA
  )

})
