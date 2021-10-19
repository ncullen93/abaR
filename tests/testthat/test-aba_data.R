test_that("set_data works", {
  m <- aba_model()

  df <- data.frame(x=c(1,2,3), y=c(1,2,3))

  m2 <- m %>% set_data(df)
  m3 <- m %>% set_data(df %>% dplyr::tibble())
  m4 <- m2 %>% set_data(df)

  expect_s3_class(m, 'abaModel')
  expect_s3_class(m2, 'abaModel')
  expect_s3_class(m3, 'abaModel')
  expect_s3_class(m4, 'abaModel')

  expect_error(
    m %>% set_data(c(1,2,3))
  )
  expect_error(
    m %>% set_data(1)
  )

})
