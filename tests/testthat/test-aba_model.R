test_that("creation works", {
  m <- aba_model()
  expect_s3_class(m, 'abaModel')
})


test_that("different predictor forms work", {
  df <- aba::adnimerge %>% filter(VISCODE == 'bl')

  expect_error(
    model <- df %>% aba_model() %>%
      set_groups(everyone()) %>%
      set_outcomes(ConvertedToAlzheimers) %>%
      set_predictors(
        c('CSF_ABETA_bl','CSF_PTAU_bl','CSF_TAU_bl')
      ) %>%
      set_stats('lm') %>%
      aba_fit(),
    NA
  )

  expect_output(
    print(model),
    NULL
  )

  # three individual biomarkers
  vars <- c('CSF_ABETA_bl','CSF_PTAU_bl','CSF_TAU_bl')
  expect_error(
    model2 <- df %>% aba_model() %>%
      set_groups(everyone()) %>%
      set_outcomes(ConvertedToAlzheimers) %>%
      set_predictors(vars) %>%
      set_stats('lm') %>%
      aba_fit(),
    NA
  )

})
