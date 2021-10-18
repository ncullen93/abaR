test_that("creation works", {
  m <- aba_model()
  expect_s3_class(m, 'abaModel')
})
