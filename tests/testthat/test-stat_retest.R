test_that("example works", {

  data <- adnimerge %>%
    dplyr::filter(
      VISCODE %in% c('bl' ,'m06', 'm12'),
      DX_bl == 'CU'
    )
  model <- data %>% aba_model() %>%
    set_groups(
      everyone(),
      CSF_ABETA_STATUS_bl == 1,
      labels = c('CU', 'CU AB-')
    ) %>%
    set_outcomes(
      ADAS13, MMSE,
      labels = c('ADAS13', 'MMSE')
    ) %>%
    set_stats(
      stat_retest(id = 'RID', time = 'VISCODE')
    ) %>%
    fit()

  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        everyone(),
        CSF_ABETA_STATUS_bl == 1,
        labels = c('CU', 'CU AB-')
      ) %>%
      set_outcomes(
        ADAS13, MMSE,
        labels = c('ADAS13', 'MMSE')
      ) %>%
      set_stats(
        stat_retest(id = 'RID', time = 'VISCODE')
      ) %>%
      fit(),
    NA
  )

  expect_error(
    model_summary <- model %>% aba_summary(),
    NA
  )

  expect_error(
    g <- model_summary %>%
      aba_plot_coef(axis=c('term','group','outcome','predictor'),
                    coord_flip=T),
    NA
  )

})


test_that("accidental grouped data as input still works", {

  data <- adnimerge %>%
    dplyr::filter(
      VISCODE %in% c('bl' ,'m06', 'm12'),
      DX_bl == 'CU'
    ) %>%
    group_by(CSF_ABETA_STATUS_bl)

  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        everyone(),
        CSF_ABETA_STATUS_bl == 1,
        labels = c('CU', 'CU AB-')
      ) %>%
      set_outcomes(
        ADAS13, MMSE,
        labels = c('ADAS13', 'MMSE')
      ) %>%
      set_stats(
        stat_retest(id = 'RID', time = 'VISCODE')
      ) %>%
      aba_fit(),
    NA
  )
})


