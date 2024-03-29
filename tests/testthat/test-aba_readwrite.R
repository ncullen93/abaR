test_that("example works", {
  # create temp files to save to
  tmp_filename_csv <- tempfile(fileext = '.csv')
  tmp_filename_rda <- tempfile(fileext = '.Rda')
  tmp_filename_xlsx <- tempfile(fileext = '.xlsx')

  # grab built-in data
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  # fit model
  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats('glm') %>%
    fit()

  # summarise model
  model_summary <- model %>% summary()

  # save model summary to file as table
  expect_error(
    model_summary %>% aba_write(tmp_filename_csv),
    NA
  )

  # save model summary to file as raw long-form results
  expect_error(
    model_summary %>% aba_write(tmp_filename_csv, format = 'raw'),
    NA
  )

  # save model summary as an object which can be loaded back into memory
  expect_error(
    model_summary %>% aba_write(tmp_filename_rda, format = 'object'),
    NA
  )

  # load summary back to file to show it works
  expect_error(
    model_summary2 <- aba_read(tmp_filename_rda),
    NA
  )

  expect_error(
    model_summary %>% aba_write(tmp_filename_xlsx),
    NA
  )

  expect_error(
    model_summary %>% aba_write(tmp_filename_xlsx, split=TRUE),
    NA
  )

  # delete temp files
  removed <- file.remove(tmp_filename_csv)
  removed <- file.remove(tmp_filename_rda)
  removed <- file.remove(tmp_filename_xlsx)
})

test_that("save and load works", {
  df <- adnimerge %>% filter(VISCODE=='bl')

  model <- df %>% aba_model() %>%
    set_groups("DX_bl == 'MCI'", everyone()) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_predictors(
      PLASMA_ABETA_bl,
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_outcomes(CSF_ABETA_STATUS_bl, ConvertedToAlzheimers) %>%
    set_stats('glm') %>%
    aba_fit()

  model_spec <- model$spec

  expect_error(
    model,
    NA
  )
})
