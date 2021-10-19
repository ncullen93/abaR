test_that("creation works", {
  spec <- aba_spec()
  expect_s3_class(spec, 'abaSpec')
})

test_that('set_groups works', {
  m <- aba_model() %>% set_data(data.frame(c(1,2,3))) %>% set_groups()
  expect_s3_class(m, 'abaModel')
})

test_that('set_outcomes works', {
  m <- aba_model() %>% set_data(data.frame(c(1,2,3))) %>% set_outcomes()
  expect_s3_class(m, 'abaModel')
})

test_that('set_covariates works', {
  m <- aba_model() %>% set_data(data.frame(c(1,2,3))) %>% set_covariates()
  expect_s3_class(m, 'abaModel')
})

test_that('set_predictors works', {
  m <- aba_model() %>% set_data(data.frame(c(1,2,3))) %>% set_predictors()
  expect_s3_class(m, 'abaModel')
})
