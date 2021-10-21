test_that("fit with string inputs works", {
  expect_error(
    aba_model() %>%
      set_groups(
        "DX_bl == 'CU'",
        'TRUE'
      ) %>%
      set_outcomes(
        'ConvertedToAlzheimers',
        'ConvertedToDementia'
      ) %>%
      set_predictors(
        'PLASMA_ABETA_bl',
        'PLASMA_PTAU181_bl',
        'PLASMA_NFL_bl',
        c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
      ) %>%
      set_covariates(
        'AGE_bl', 'GENDER', 'EDUCAT'
      ) %>%
      set_stats(
        'glm'
      ) %>%
      set_data(adni_sample) %>%
      fit(),
    NA
  )
})

test_that("fit with tidy eval inputs works", {
  expect_error(
    adni_sample %>%
      aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        TRUE
      ) %>%
      set_outcomes(
        ConvertedToAlzheimers,
        ConvertedToDementia
      ) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl),
      ) %>%
      set_covariates(
        AGE_bl, GENDER, EDUCAT
      ) %>%
      set_stats('glm') %>%
      fit(),
    NA
  )
})

test_that("tidy eval before setting data gives error", {
  # never setting data
  expect_error(
    aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        TRUE
      ) %>%
      set_outcomes(
        ConvertedToAlzheimers,
        ConvertedToDementia
      ) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(
        AGE_bl, GENDER, EDUCAT
      ) %>%
      set_stats(
        'glm'
      ) %>%
      fit()
  )

  # setting data after spec parameters
  expect_error(
    aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        TRUE
      ) %>%
      set_outcomes(
        ConvertedToAlzheimers,
        ConvertedToDementia
      ) %>%
      set_predictors(
        PLASMA_ABETA_bl,
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(
        AGE_bl, GENDER, EDUCAT
      ) %>%
      set_stats(
        'glm'
      ) %>%
      set_data(adni_sample) %>%
      fit()
  )
})
