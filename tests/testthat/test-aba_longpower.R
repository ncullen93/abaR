test_that("example works", {

  # use only two year follow-up data; filter by some basic AD trial criteria
  data <- adnimerge %>%
    dplyr::filter(
      VISCODE %in% c('bl', 'm06', 'm12', 'm24'),
      DX_bl %in% c('MCI','AD'),
      CDR_bl %in% c(0.5, 1),
      MMSE_bl >= 20, MMSE_bl <= 28
    )
  # fit an aba model with an lme stat to get a longitudinal model
  model <- data %>% aba_model() %>%
    set_outcomes(CDRSB, ADAS13) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats(stat_lme(id = 'RID', time = 'YEARS_bl')) %>%
    fit()
  # summarize aba model - not necessary here but good to see results
  model_summary <- model %>% summary()
  # run power analysis on the fitted aba model with various assumptions
  # e.g., treatment effect between 25 - 35%; power between 80 - 90%
  expect_error(
    pwr <- model %>%
      aba_longpower(
        n = NULL,
        pct_change = c(0.25, 0.30, 0.35),
        power = c(0.8, 0.85, 0.9),
        t_length = 2,
        t_freq = 0.25,
        dropout = 0.2
      ),
    NA
  )

  expect_output(
    print(pwr),
    NULL
  )

  # generate a standard results figure from the power results
  expect_error(
    fig <- pwr %>% aba_plot(),
    NA
  )


  # add better inclusion criteria (CSF & CSF+MRI) to the aba model and refit.
  model2 <- model %>%
    set_groups(
      everyone(),
      (CSF_ABETA_bl < 880) & (CSF_PTAU_bl > 24),
      (CSF_ABETA_bl < 880) & (CSF_PTAU_bl > 24) & (MRI_HIPP_bl < 6000),
      labels = c('DX + COG', 'DX + COG + CSF', 'DX + COG + CSF + MRI')
    ) %>%
    fit()
  # summarise model fit - again, not necessary but good to see slopes
  model2_summary <- model2 %>% summary()

  expect_error(
    pwr2 <- model2 %>%
      aba_longpower(
        n = NULL,
        pct_change = c(0.25, 0.3, 0.35),
        power = c(0.8, 0.85, 0.9),
        t_length = 2,
        t_freq = 0.25,
        dropout = 0.2
      ),
    NA
  )
  expect_error(
    fig2 <- pwr2 %>% aba_plot(),
    NA
  )
})
