test_that("creation works", {
  m <- aba_trial()
  expect_s3_class(m, 'abaTrial')
})


