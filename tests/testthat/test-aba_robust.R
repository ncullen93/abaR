test_that("example works", {
  # read and process data
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit a standard model to predict a binary outcome
  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(CSF_ABETA_STATUS_bl) %>%
    set_predictors(PLASMA_PTAU181_bl, PLASMA_NFL_bl) %>%
    set_stats(stat_roc(method='Youden', direction = '<')) %>%
    aba_fit()
  # summarise model (these are the original results)
  model_summary <- model %>% aba_summary()
  # specify test-retest variation for predictors (defined as percent change)
  # this can be theoretical values (e.g. 5, 10, 15, 20) or derived from
  # test-retest studies where you measured the biomarkers twice
  variation <- list(
    'PLASMA_PTAU181_bl' = 9.5,
    'PLASMA_NFL_bl' = 20.2
  )
  # test robustness of the fitted aba model to this robustness
  expect_error(
    model_robust <- model %>%
      aba_robust(
        variation = variation,
        ntrials = 5,
        verbose = TRUE
      ),
    NA
  )

  # plot results using the generic plot function
  expect_error(
    fig <- model_robust %>% aba_plot_metric(),
    NA
  )

})
