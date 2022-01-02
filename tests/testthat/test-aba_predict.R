test_that("example works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  model <- data %>% aba_model() %>%
    set_groups(
      everyone(),
      DX_bl %in% c('MCI', 'AD')
    ) %>%
    set_outcomes(CDRSB_bl, MMSE_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('lm')
  model <- model %>% fit()
  # add model predictions to original data
  expect_error(
    data_augmented <- model %>% aba_predict(),
    NA
  )
  # store predictions separately by group - outcome - stat combination
  expect_error(
    data_augmented2 <- model %>% aba_predict(merge = FALSE),
    NA
  )
})
