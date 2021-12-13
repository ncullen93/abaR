test_that("example works", {
  df <- adnimerge %>%
    dplyr::filter(
      VISCODE %in% c('bl','m06','m12','m24'),
      !is.na(CSF_ABETA_STATUS_bl),
      DX_bl %in% c('MCI')
    ) %>%
    dplyr::mutate(
      TREATMENT = factor(CSF_ABETA_STATUS_bl, levels=c(0,1),
                         labels=c('Placebo','Treatment')),
      ADAS13 = ADAS13 - ADAS13_bl,
      CDRSB = CDRSB - CDRSB_bl,
      MMSE = MMSE - MMSE_bl
    )
  # fit mmrm model for different endpoints, adjusted for covariates
  model <- df %>% aba_model() %>%
    set_outcomes(CDRSB, ADAS13) %>%
    set_covariates(
      AGE, GENDER, EDUCATION
    ) %>%
    set_stats(
      stat_mmrm(id = 'RID', time = 'VISCODE', treatment = 'TREATMENT')
    ) %>%
    aba_fit()

  # run emmeans
  skip_on_cran()
  expect_error(
    model_emmeans <- model %>% aba_emmeans(),
    NA
  )

  expect_error(
    figs <- model_emmeans %>% aba_plot(),
    NA
  )
})
