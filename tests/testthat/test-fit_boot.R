test_that("fit_boot works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  model_spec <- aba_model() %>%
    set_data(data) %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats('glm')

  expect_error(
    model <- model_spec %>% fit_boot(ntrials = 5),
    NA
  )

})
