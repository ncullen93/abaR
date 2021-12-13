
test_that("pvalue adjust works", {

  get_pvals <- function(ms) {
    list(
      'metrics' = ms$results$metrics %>%
        filter(term=='pval') %>%
        pull(estimate),
      'coefs' = ms$results$coefs$pval
    )
  }
  p_ratio <- function(pnew, pold) {
    is_one <- (pnew == 1) | (pnew == 0)
    pnew <- pnew[!is_one]
    pold <- pold[!is_one]
    x <- unique(pnew / pold)
    x[!x %in% c(NaN, 1)]
  }

  df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
  model <- df %>% aba_model() %>%
    set_groups(DX_bl=='CU', DX_bl=='MCI') %>%
    set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
    set_predictors(
      PLASMA_PTAU181_bl, PLASMA_NFL_bl,
      c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
    ) %>%
    set_stats('glm') %>%
    fit()

  # no adjustment
  ms <- model %>% aba_summary()
  pvals <- get_pvals(ms)

  # default - correct within group, outcome, and stat
  # 3 model pvals, 4 coefficient pvals
  ms2 <- model %>% aba_summary(adjust = aba_adjust(method='bonferroni'))
  pvals2 <- get_pvals(ms2)

  expect_equal(length(pvals$metrics), length(pvals2$metrics))
  expect_equal(length(pvals$coefs), length(pvals2$coefs))

  expect_equal(p_ratio(pvals2$metrics, pvals$metrics), 3)
  expect_equal(p_ratio(pvals2$coefs, pvals$coefs), 4)

  # correct within group but across outcomes
  # 6 model pvals, 8 coefficient pvals
  ms3 <- model %>% aba_summary(adjust=aba_adjust(method='bonferroni', by=c('group')))
  pvals3 <- get_pvals(ms3)

  expect_equal(length(pvals$metrics), length(pvals3$metrics))
  expect_equal(length(pvals$coefs), length(pvals3$coefs))
  expect_equal(p_ratio(pvals3$metrics, pvals$metrics), 6)
  expect_equal(p_ratio(pvals3$coefs, pvals$coefs), 8)

  # correct only model P-values, not coefficient P-values
  ms4 <- model %>% aba_summary(adjust=aba_adjust(method='b', form = 'metric'))
  pvals4 <- get_pvals(ms4)
  expect_equal(length(pvals$metrics), length(pvals4$metrics))
  expect_equal(length(pvals$coefs), length(pvals4$coefs))
  expect_equal(p_ratio(pvals4$metrics, pvals$metrics), 3)
  expect_equal(p_ratio(pvals4$coefs, pvals$coefs), numeric(0))

  # correct only coef P-values, not model P-values
  ms4 <- model %>% aba_summary(adjust=aba_adjust(method='b', form = 'coef'))
  pvals4 <- get_pvals(ms4)
  expect_equal(length(pvals$metrics), length(pvals4$metrics))
  expect_equal(length(pvals$coefs), length(pvals4$coefs))
  expect_equal(p_ratio(pvals4$metrics, pvals$metrics), numeric(0))
  expect_equal(p_ratio(pvals4$coefs, pvals$coefs), 4)

})
