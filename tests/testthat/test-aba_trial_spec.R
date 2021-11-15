test_that("creation works", {
  s <- aba_trial_spec()
  expect_s3_class(s, "abaTrialSpec")
})
