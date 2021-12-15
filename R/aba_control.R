#' Create an aba control object.
#'
#' The aba control which determines how an aba summary will be calculated and
#' printed to console.
#'
#' @param include_covariates boolean. Whether to include covariates in coefs
#' @param include_intercept  boolean. Whether to include intercept in coefs
#' @param pval_digits integer. How many decimals of a pvalue to show
#' @param aic_digits integer. How many decimals of AIC value to show
#' @param metric_digits integer. Default value of how many decimals to show
#'   for model metrics (e.g., auc, adj.r.squared, etc)
#' @param coef_digits integer. Default value of how many decimals to show
#'   for model coefficients
#' @return a list with the control parameters specified
#' @export
#'
#' @examples
#'
#' df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # standard example
#' model <- df %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('glm') %>%
#'   aba_fit()
#'
#' # no control -> default
#' model_summary <- model %>% aba_summary()
#' print(model_summary)
#'
#' # add a control object - don't include covariate coefficients
#' my_control <- aba_control(include_covariates = FALSE)
#' model_summary2 <- model %>% aba_summary(control = my_control)
#' print(model_summary2)
#'
aba_control <- function(include_intercept = FALSE,
                        include_covariates = TRUE,
                        pval_digits = 4,
                        aic_digits = 0,
                        metric_digits = 2,
                        coef_digits = 2) {

  ctrl <- list(
    include_covariates = include_covariates,
    include_intercept = include_intercept,
    pval_digits = pval_digits,
    aic_digits = aic_digits,
    metric_digits = metric_digits,
    coef_digits = coef_digits
  )

  class(ctrl) <- 'abaControl'

  return(ctrl)
}
