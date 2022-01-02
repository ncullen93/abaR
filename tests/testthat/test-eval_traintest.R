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
    set_stats('lm') %>%
    set_evals(
      eval_traintest(split = 0.8)
    )

  expect_error(
    model <- model %>% fit(),
    NA
  )

  expect_error(
    s <- model %>% summary(),
    NA
  )
})
