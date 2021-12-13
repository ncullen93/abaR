test_that("example works", {
  df <- aba::adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # standard model selection
  model <- df %>% aba_model() %>%
    set_outcomes(ConvertedToAlzheimers) %>%
    set_predictors(
      CDRSB_bl, ADAS13_bl, MMSE_bl,
      CSF_ABETA_bl, CSF_PTAU_bl, CSF_TAU_bl
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('glm') %>%
    aba_fit()
  model_summary <- model %>% aba_summary()
  # default selection - forward selection by AIC with threshold = -2
  skip_on_cran()
  expect_error(
    model_selection <- model %>% aba_selection(verbose=F),
    NA
  )
  # selection with p-value and threshold = 0.1
  expect_error(
    model_selection <- model %>%
      aba_selection(criteria = 'pval', threshold=0.1, verbose=F),
    NA
  )

  expect_output(
    print(model_selection),
    NULL
  )
})
