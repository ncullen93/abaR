test_that("example works", {
  # create separate training and testing datasets
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  train_id <- sample(nrow(data), 0.8*nrow(data), replace = FALSE)
  data_train <- data %>% dplyr::filter(dplyr::row_number() %in% train_id)
  data_test <- data %>% dplyr::filter(dplyr::row_number() %in% train_id)
  # fit model on train data
  model <- aba_model() %>%
    set_data(data_train) %>%
    set_groups(
      everyone(),
      DX_bl %in% c('MCI', 'AD')
    ) %>%
    set_outcomes(CDRSB_bl, MMSE_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('lm') %>%
    fit()
  # evaluate fitted model on test data
  expect_error(
    res <- model %>% aba_evaluate(data = data_test),
    NA
  )
})
