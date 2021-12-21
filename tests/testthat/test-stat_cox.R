test_that("example works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(everyone()) %>%
      set_outcomes(ConvertedToAlzheimers, ConvertedToDementia) %>%
      set_predictors(
        PLASMA_PTAU181_bl, PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_stats(
        stat_cox(time = 'TimeUnderRiskDementia')
      ) %>%
      fit(),
    NA
  )

  expect_error(
    model_summary <- model %>% summary(),
    NA
  )

})
