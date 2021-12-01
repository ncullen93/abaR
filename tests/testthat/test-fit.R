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
        'AGE', 'GENDER', 'EDUCATION'
      ) %>%
      set_stats(
        'glm'
      ) %>%
      set_data(adnimerge) %>%
      fit(),
    NA
  )
})

test_that("fit with tidy eval inputs works", {
  expect_error(
    adnimerge %>%
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
        AGE, GENDER, EDUCATION
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
        AGE, GENDER, EDUCATION
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
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        'glm'
      ) %>%
      set_data(adnimerge) %>%
      fit()
  )
})


test_that("fit with only one value for each spec param works", {

  # no group & no predictors - should still work
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_outcomes(MMSE) %>%
      set_covariates(AGE) %>%
      set_stats('lm') %>%
      fit(),
    NA
  )

  # no group & no covariates - should still work
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_outcomes(MMSE) %>%
      set_covariates(AGE) %>%
      set_stats('lm') %>%
      fit(),
    NA
  )
})

test_that("fit with stat objects instead of strings works", {

  # one aba stat object
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        everyone()
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
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        aba_glm()
      ) %>%
      fit(),
    NA
  )

  # two aba stat objects
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        everyone()
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
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        aba_glm(),
        aba_lm()
      ) %>%
      fit(),
    NA
  )

  # one aba stat object and one string
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        everyone()
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
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        aba_glm(),
        'lm'
      ) %>%
      fit(),
    NA
  )

})


