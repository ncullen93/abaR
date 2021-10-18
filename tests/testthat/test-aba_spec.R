test_that("creation works", {
  spec <- aba_spec()

  expect_s3_class(spec, 'abaSpec')
})

test_that('set_groups works', {
  m <- aba_model() %>% set_groups()
  expect_s3_class(m, 'abaModel')
})

test_that('set_outcomes works', {
  m <- aba_model() %>% set_outcomes()
  expect_s3_class(m, 'abaModel')
})

test_that('set_covariates works', {
  m <- aba_model() %>% set_covariates()
  expect_s3_class(m, 'abaModel')
})

test_that('set_predictors works', {
  m <- aba_model() %>% set_predictors()
  expect_s3_class(m, 'abaModel')
})
