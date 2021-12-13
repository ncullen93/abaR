#
#devtools::load_all()
#
#data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#
#model <- data %>% aba_model() %>%
#  set_groups(everyone()) %>%
#  set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#  set_predictors(
#    PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#    c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#  ) %>%
#  set_stats(
#    stat_glm(std.beta = TRUE)
#  ) %>%
#  fit()
#
#
#check_pval_digits <- function(ms, digits) {
#  ms %>% as_table() %>% pull('pval') %>%
#    map_int(~nchar(.)!=(digits+2)) %>% sum() == 0
#}
#
#ms <- model %>% aba_summary()
#check_pval_digits(ms, 4)
#
#my_ctrl <- aba_control(pval_digits = 4)
#ms2 <- model %>% aba_summary(control = my_ctrl)
#check_pval_digits(ms2, 4)
#
#
#my_ctrl <- aba_control(pval_digits = 3)
#ms2 <- model %>% aba_summary(control = my_ctrl)
#
#my_ctrl <- aba_control(pval_digits = 5)
#ms2 <- model %>% aba_summary(control = my_ctrl)
#


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
