test_that("creation works", {
  s <- trial_spec()
  expect_s3_class(s, "trialSpec")
})
