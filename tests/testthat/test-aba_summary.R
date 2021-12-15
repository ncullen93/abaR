
test_that("summarizing a non-fitted model gives an informative error", {
  m <- adnimerge %>% aba_model() %>%
    set_groups(DX_bl == 'CU') %>%
    set_outcomes(ConvertedToAlzheimers, ConvertedToDementia) %>%
    set_predictors(
      PLASMA_PTAU181_bl,
      PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats('glm')

  expect_error(
    ms <- m %>% aba_summary(),
    'not been fit'
  )

  # fitting then changing a parameter should make it not fit anymore
  m <- m %>% aba_fit()
  ms <- m %>% aba_summary()

  m <- m %>% set_covariates(AGE, GENDER)
  expect_error(
    ms <- m %>% aba_summary(),
    'not been fit'
  )

})

test_that("standard summary works", {
  expect_error(
    m <- adnimerge %>% aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        everyone()
      ) %>%
      set_outcomes(
        ConvertedToAlzheimers,
        ConvertedToDementia
      ) %>%
      set_predictors(
        PLASMA_PTAU181_bl,
        PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        'glm'
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )

  expect_error(
    tbl <- m %>% as_table(),
    NA
  )

  expect_error(
    tbl <- m %>% as_reactable(),
    NA
  )

  expect_output(
    print(m),
    NULL
  )
})
