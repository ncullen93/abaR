library(testthat)
library(aba)

expect_works <- function(object) {
  expect_error(object, NA)
}

test_check("aba")
