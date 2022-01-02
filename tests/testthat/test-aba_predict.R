test_that("example works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  model <- data %>% aba_model() %>%
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
    set_stats('lm')
  model <- model %>% fit()
  # add model predictions to original data
  expect_error(
    data_augmented <- model %>% aba_predict(),
    NA
  )
  # store predictions separately by group - outcome - stat combination
  expect_error(
    data_augmented2 <- model %>% aba_predict(merge = FALSE),
    NA
  )
})

test_that("newdata works", {
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
    res1 <- model %>% aba_predict(),
    NA
  )
  expect_error(
    res2 <- model %>% aba_predict(newdata = data_test),
    NA
  )
})
