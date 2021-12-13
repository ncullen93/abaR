

test_that("complete.cases=F for lm works", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit lm model with continuous outcome variables
  expect_error(
    model <- data %>% aba_model() %>%
      set_groups(
        everyone(),
        DX_bl %in% c('MCI', 'AD')
      ) %>%
      set_outcomes(CDRSB_bl, MMSE_bl) %>%
      set_predictors(
        PLASMA_PTAU181_bl, PLASMA_NFL_bl,
        c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
      ) %>%
      set_covariates(AGE, GENDER, EDUCATION) %>%
      set_stats(
        stat_lm(complete.cases=F)
      ) %>%
      aba_fit() %>%
      aba_summary(),
    NA
  )
})

test_that("std.beta works as intended for stat_lm", {
  data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  # fit lm model with continuous outcome variables
  model <- data %>% aba_model() %>%
    set_groups(
      DX_bl %in% c('MCI', 'AD')
    ) %>%
    set_outcomes(CDRSB_bl, MMSE_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_covariates(AGE, GENDER, EDUCATION)

  expect_error(
    m1 <- model %>%
      set_stats(
        stat_lm(std.beta = T)
      ) %>%
      fit() %>%
      summary(),
    NA
  )

  expect_error(
    m2 <- model %>%
      set_stats(
        stat_lm(std.beta = F)
      ) %>%
      fit() %>%
      summary(),
    NA
  )

  expect_false(isTRUE(all.equal(
    m1$results$coefs$estimate,
    m2$results$coefs$estimate
  )))

  expect_error(
    m3 <- model %>%
      set_stats(
        stat_lm(std.beta = c(T,F))
      ) %>%
      fit() %>%
      summary(),
    NA
  )

  expect_false(isTRUE(all.equal(
    m1$results$coefs$estimate,
    m3$results$coefs$estimate
  )))
  expect_false(isTRUE(all.equal(
    m2$results$coefs$estimate,
    m3$results$coefs$estimate
  )))

  expect_error(
    m4 <- model %>%
      set_stats(
        stat_lm(std.beta = c(F,T))
      ) %>%
      fit() %>%
      summary(),
    NA
  )
  expect_false(isTRUE(all.equal(
    m1$results$coefs$estimate,
    m4$results$coefs$estimate
  )))
  expect_false(isTRUE(all.equal(
    m2$results$coefs$estimate,
    m4$results$coefs$estimate
  )))
  expect_false(isTRUE(all.equal(
    m3$results$coefs$estimate,
    m4$results$coefs$estimate
  )))
})
