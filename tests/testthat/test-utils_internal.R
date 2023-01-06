# this is an internal function that is very important
# for determining how user input to set_* functions gets parsed
test_that("parse_select_expr works as expected", {
  x <- parse_select_expr(
    PLASMA_PTAU181_bl,
    PLASMA_NFL_bl,
    c(PLASMA_PTAU181_bl, PLASMA_NFL_bl),
    data = aba::adnimerge
  )

  expect_equal(
    ignore_attr = TRUE,
    x,
    list(
      'PLASMA_PTAU181_bl',
      'PLASMA_NFL_bl',
      c('PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
    )
  )
})
