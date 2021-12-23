test_that("eval_traintest works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  model <- aba_model() %>%
    set_data(data) %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats('glm') %>%
    set_evals(
      eval_traintest(split = 0.8),
      eval_traintest(split = 0.8, ntrials = 10)
    )

  expect_error(
    model <- model %>% fit(),
    NA
  )
})
