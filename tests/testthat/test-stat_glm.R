test_that("complete.cases=F works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        everyone(),
        DX_bl %in% c('MCI', 'AD')
      ) %>%
      set_outcomes(CSF_ABETA_STATUS_bl, ConvertedToAlzheimers) %>%
      set_predictors(
        PLASMA_PTAU181_bl, PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        stat_glm(complete.cases=F, std.beta=T)
      ) %>%
      aba_fit(),
    NA
  )

  expect_error(
    ms <- model %>% aba_summary(),
    NA
  )

  expect_error(
    g <- ms %>% aba_plot_metric(),
    NA
  )

  expect_error(
    g <- ms %>% aba_plot_coef(),
    NA
  )

  expect_error(
    g <- ms %>% aba_plot_coef(coord_flip=T),
    NA
  )

  expect_error(
    g <- ms %>% aba_plot_roc(),
    NA
  )
})

test_that("plot risk density example works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit glm model with binary outcome variables
  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats(
      stat_glm(std.beta = TRUE)
    ) %>%
    fit()

  expect_error(
    fig <- model %>% aba_plot_risk_density(),
    NA
  )
})


test_that("plot predictor risk example works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit glm model with binary outcome variables
  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats(
      stat_glm(std.beta = FALSE)
    ) %>%
    fit()

  expect_error(
    g <- model %>% aba_plot_predictor_risk(),
    NA
  )
})
