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
      aba_fit(),
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
      aba_fit(),
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
      aba_fit()
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
      aba_fit()
  )
})


test_that("small data after processing throws an error", {

  data_start <- adnimerge %>%
    dplyr::filter(VISCODE == 'bl',
                  DX_bl == 'MCI') %>%
    select(DX_bl, MMSE_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
           AGE, GENDER, EDUCATION) %>%
    filter(complete.cases(.))

  ###############
  data_missing <- data_start
  data_missing$MMSE_bl <- NA

  expect_error(
    model <- data_missing %>%
      aba_model() %>%
      set_outcomes(MMSE_bl) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats('lm') %>%
      aba_fit()
  )

  ###############
  data_missing <- data_start
  data_missing$PLASMA_PTAU181_bl <- NA

  expect_error(
    model <- data_missing %>%
      aba_model() %>%
      set_outcomes(MMSE_bl) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats('lm') %>%
      aba_fit()
  )

  #########
  # this should not give an error because nrow(data) < 10 (= 9)
  data_missing <- data_start
  data_missing[1:(nrow(data_missing)-9),'PLASMA_PTAU181_bl'] <- NA
  expect_error(
    model <- data_missing %>%
      aba_model() %>%
      set_outcomes(MMSE_bl) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats('lm') %>%
      aba_fit()
  )

  #########

  # this should not give an error because nrow(data) > 10 (= 11)
  data_missing <- data_start
  data_missing[1:(nrow(data_missing)-11),'PLASMA_PTAU181_bl'] <- NA
  expect_error(
    model <- data_missing %>%
      aba_model() %>%
      set_outcomes(MMSE_bl) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats('lm') %>%
      aba_fit(),
    NA
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
      aba_fit(),
    NA
  )

  # no group & no covariates - should still work
  expect_error(
    m <- adnimerge %>%
      aba_model() %>%
      set_outcomes(MMSE) %>%
      set_covariates(AGE) %>%
      set_stats('lm') %>%
      aba_fit(),
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
        stat_glm()
      ) %>%
      aba_fit(),
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
        stat_glm(),
        stat_lm()
      ) %>%
      aba_fit(),
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
        stat_glm(),
        'lm'
      ) %>%
      aba_fit(),
    NA
  )

})


