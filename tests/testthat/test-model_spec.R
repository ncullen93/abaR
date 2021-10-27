test_that("creation works", {
  spec <- model_spec()
  expect_s3_class(spec, 'modelSpec')
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


test_that("set_covariates with strings", {
  m <- aba_model()
  # string / no data -> should work
  expect_error(
    m2 <- m %>% set_covariates('a','b','c'),
    NA
  )
  # string / data -> should throw error if variable(s) doesnt exist
  expect_error(
    m %>% set_data(adni_sample) %>% set_covariates('a','b','c')
  )
  # should work if variable(s) all exist
  expect_error(
    m2 <- m %>% set_data(adni_sample) %>% set_covariates('AGE_bl','GENDER','EDUCAT'),
    NA
  )
})

test_that("tidy eval throws error without data set", {
  # set_predictors()
  expect_error(
    aba_model() %>% set_predictors('x','y',z)
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',c(x,y))
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',contains('x'))
  )

  # set_groups()
  expect_error(
    aba_model() %>% set_groups(x > 1)
  )
})

test_that("string eval works without data set", {
  # set_predictors()
  expect_error(
    aba_model() %>% set_predictors('x','y','z'),
    NA
  )
  expect_error(
    aba_model() %>% set_predictors('x','y',c('x','y')),
    NA
  )
  # set_groups()
  expect_error(
    aba_model() %>% set_groups('x > 1'),
    NA
  )
})




