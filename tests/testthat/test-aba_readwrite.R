test_that("save and load works", {
  df <- adni_sample %>% filter(VISIT==0)

  model <- df %>% aba_model() %>%
    set_groups("DX_bl == 'MCI'", everyone()) %>%
    set_covariates(AGE_bl, GENDER, EDUCAT) %>%
    set_predictors(
      PLASMA_ABETA_bl,
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_outcomes(CSF_ABETA_STATUS_bl, ConvertedToAlzheimers) %>%
    set_stats('glm') %>%
    fit()

  model_spec <- model$spec

  # create temp dir to save to


  #expect_error(
  #  model_spec %>% aba_write('~/desktop/myspec.rda'),
  #  NA
  #)
  #model_spec %>% aba_write('~/desktop/myspec.rda')

  #expect_error(
  #  model_spec2 <- aba_read('~/desktop/myspec.rda'),
  #  NA
  #)
  #model_spec2 <- aba_read('~/desktop/myspec.rda')
  #model2 <- df %>% aba_model(spec=model_spec2) %>% fit()

  ## save model
  #expect_error(
  #  model %>% aba_write('~/desktop/mymodel.rda',
  #                     include_data = FALSE, include_fit = FALSE),
  #  NA
  #)
  #model %>% aba_write('~/desktop/mymodel.rda',
  #                   include_data = FALSE, include_fit = FALSE)
  #expect_error(
  #  model3 <- aba_read('~/desktop/mymodel.rda'),
  #  NA
  #)
  #model3 <- aba_read('~/desktop/mymodel.rda')
  #model3 <- model3 %>% set_data(df) %>% fit()

})
