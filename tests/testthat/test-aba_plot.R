test_that("theme_aba works", {
  data <- data.frame(x=1:10, y=1:10)
  fig <- ggplot(data, aes(x=x,y=y)) + geom_point()
  expect_error(
    fig <- fig + theme_aba(),
    NA
  )
})
