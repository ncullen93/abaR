
test_that("digits works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats(
      stat_glm(std.beta=T)
    ) %>%
    fit()

  check_digits <- function(ms, metric, digits) {
    ms %>% as_table() %>% pull({{ metric }}) %>%
      map_int(~nchar(strsplit(.,'\\.')[[1]][[2]])) %>%
      map_lgl(~.!=digits) %>% sum() == 0
  }

  ms <- model %>% aba_summary()
  expect_true(check_digits(ms, 'pval', 4))
  expect_identical(
    ms %>% as_table() %>% pull('aic') %>% head(1),
    '874'
  )
  my_ctrl <- aba_control(pval_digits = 5, aic_digits = 1)
  ms2 <- model %>% aba_summary(control = my_ctrl)
  expect_true(check_digits(ms2, 'pval', 5))
  expect_true(check_digits(ms2, 'aic', 1))

  my_ctrl <- aba_control(pval_digits = 3, metric_digits = 3, aic_digits=2)
  ms2 <- model %>% aba_summary(control = my_ctrl)
  expect_true(check_digits(ms2, 'pval', 3))
  expect_true(check_digits(ms2, 'aic', 2))
  expect_identical(
    ms2 %>% as_table() %>% pull('auc') %>% head(1),
    "0.576 [0.532, 0.621]"
  )

  # coefficients
  my_ctrl <- aba_control(coef_digits = 3)
  ms2 <- model %>% aba_summary(control = my_ctrl)
  expect_identical(
    ms2 %>% as_table() %>% pull(PLASMA_NFL_bl) %>% tail(1),
    '1.121 [0.912, 1.378] (P=0.2781)'
  )

  my_ctrl <- aba_control(coef_digits = 1)
  ms2 <- model %>% aba_summary(control = my_ctrl)
  expect_identical(
    ms2 %>% as_table() %>% pull(PLASMA_NFL_bl) %>% tail(1),
    '1.1 [0.9, 1.4] (P=0.2781)'
  )

  my_ctrl <- aba_control(coef_digits = 0, pval_digits = 3)
  ms2 <- model %>% aba_summary(control = my_ctrl)
  expect_identical(
    ms2 %>% as_table() %>% pull(PLASMA_NFL_bl) %>% tail(1),
    '1 [1, 1] (P=0.278)'
  )
})


test_that('include controls work', {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')

  model <- data %>% aba_model() %>%
    set_groups(everyone()) %>%
    set_outcomes(CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION) %>%
    set_stats(
      stat_glm(std.beta=T)
    ) %>%
    fit()

  # include covariates
  ms <- model %>% aba_summary(control = aba_control(include_covariates = FALSE))
  n <- ms %>% as_table() %>% names()
  expect_equal(
    length(n[n %in% c('AGE','GENDER','EDUCATION')]), 0
  )

  # include covariates
  ms <- model %>% aba_summary(control = aba_control(include_intercept = TRUE))
  n <- ms %>% as_table() %>% names()
  expect_true("(Intercept)" %in% n)

})




