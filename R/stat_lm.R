

#' Create an lm stat to use for an aba model.
#'
#' @return
#' list of the following functions:
#'   * `formula_fn`: create a formula
#'   * `fit_fn`: fit a model
#'   * `evaluate_fn`: evaluate a model
#'
#' @export
#'
#' @examples
#' my_stat <- aba_lm()
#'
#' my_formula <- my_stat$formula_fn(
#'   outcome='ConvertedToAlzheimers',
#'   predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#'   covariates=c('AGE_bl','GENDER','EDUCAT')
#' )
#'
#' my_model <- my_stat$fit_fn(
#'   formula = my_formula,
#'   data = adni_sample
#' )
aba_lm <- function() {
  fns <- list(
    'formula_fn' = standard_formula_fn,
    'fit_fn' = aba_lm_fit,
    'evaluate_fn' = aba_lm_evaluate
  )
  fns$stat_type <- 'lm'
  class(fns) <- 'abaStat'
  return(fns)
}

# fit a lm model
aba_lm_fit <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# evaluate a lm model
aba_lm_evaluate <- function(model, data) {

}


