test_that("all_combos works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  # fit model with all combinations of three variables
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        everyone(),
        DX_bl %in% c('MCI', 'AD')
      ) %>%
      set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
      set_predictors(
        all_combos('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
      ) %>%
      set_stats(
        stat_glm(std.beta = TRUE)
      ) %>%
      fit(),
    NA
  )

  expect_error(
    model_summary <- model %>% aba_summary(),
    NA
  )

})
