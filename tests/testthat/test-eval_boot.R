test_that("eval_boot works", {
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
    set_evals(eval_boot(ntrials = 5))

  expect_error(
    model <- model %>% fit(),
    NA
  )

  expect_error(
    s <- model %>% summary(),
    NA
  )
})
