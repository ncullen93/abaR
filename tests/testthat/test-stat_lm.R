

test_that("complete.cases=F for lm works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit lm model with continuous outcome variables
  expect_error(
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
      set_stats(
        stat_lm(complete.cases=F)
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )
})
