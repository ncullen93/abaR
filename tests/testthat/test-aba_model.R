test_that("creation works", {
  m <- aba_model()
  expect_s3_class(m, 'abaModel')
})

test_that("set_data works", {
  m <- aba_model()
  df <- data.frame(x=c(1,2,3), y=c(1,2,3))
  df2 <- df %>% dplyr::tibble()

  m2 <- m %>% set_data(df)

  m3 <- m %>% set_data(df2)

  m4 <- m2 %>% set_data(df)

  #m5 <- m %>% set_data()

  expect_s3_class(m, 'abaModel')
  expect_s3_class(m2, 'abaModel')
  expect_s3_class(m3, 'abaModel')
  expect_s3_class(m4, 'abaModel')

  expect_error(
    m %>% set_data(c(1,2,3))
  )

})
