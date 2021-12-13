test_that("example works", {
  data <- adnimerge %>%
    dplyr::filter(
      VISCODE == 'm24',
      DX_bl %in% c('MCI', 'AD'),
      !is.na(CSF_ABETA_STATUS_bl)
    ) %>%
    dplyr::mutate(
      CDRSB = CDRSB - CDRSB_bl,
      ADAS13 = ADAS13 - ADAS13_bl,
      TREATMENT = factor(CSF_ABETA_STATUS_bl, levels=c(1,0),
                         labels=c('Placebo','Treatment'))
    )
  # fit model. note that baseline outcome will be added based on the
  # e.g., fits with "CDRSB" as outcome will also add "CDRSB_bl" to t
  expect_error(
    ancova_model <- data %>% aba_model() %>%
      set_outcomes(CDRSB, ADAS13) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        stat_ancova(treatment = 'TREATMENT', baseline_suffix = 'bl')
      ) %>%
      fit(),
    NA
  )

  # summarise model. treatment effect will be shown in the treatment
  expect_error(
    ancova_summary <- ancova_model %>% summary(),
    NA
  )
})
