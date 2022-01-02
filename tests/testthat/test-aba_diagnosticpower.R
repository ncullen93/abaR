test_that("example works", {
  # We fit a diagnostic model on pilot data.
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl', DX_bl == 'MCI')
  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats(
      stat_glm(std.beta = TRUE)
    ) %>%
    fit()
  # What will the delta on sensitivity be with 100 subjects
  expect_error(
    res_delta <- model %>% aba_diagnosticpower(n = 100),
    NA
  )

  # How many subjects do we need to acheive a delta of 0.1 on sensitivity
  expect_error(
    res_n <- model %>% aba_diagnosticpower(delta = 0.1),
    NA
  )
})
