test_that("all_levels works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  # all levels with one variable
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        all_levels(DX_bl)
      ),
    NA
  )
  expect_equal(length(model$groups), 3)

  # all levels with two variables
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        all_levels(DX_bl, CSF_ABETA_STATUS_bl)
      ),
    NA
  )
  expect_equal(length(model$groups), 6)

  # all levels with two variables and a separate group
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        all_levels(DX_bl, CSF_ABETA_STATUS_bl),
        DX_bl == 'CU'
      ),
    NA
  )
  expect_equal(length(model$groups), 7)

  # all levels with two variables and a separate group
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        DX_bl == 'CU',
        all_levels(DX_bl, CSF_ABETA_STATUS_bl)
      ),
    NA
  )
  expect_equal(length(model$groups), 7)

})
