test_that("standard mmrm works", {

  # fit
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
  expect_error(
    model <- df %>% aba_model() %>%
      set_outcomes(CDRSB, ADAS13, MMSE) %>%
      set_covariates(
        AGE, GENDER, EDUCATION
      ) %>%
      set_stats(
        stat_mmrm(id = 'RID', time = 'VISCODE', treatment = 'TREATMENT')
      ) %>%
      aba_fit(),
    NA
  )

  expect_error(
    model_summary <- model %>% aba_summary(),
    NA
  )

})
