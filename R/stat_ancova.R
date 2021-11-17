#' Create an ancova stat to use for an aba model.
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
#' my_stat <- aba_ancova()
#'
#' my_formula <- my_stat$formula_fn(
#'   outcome='PLASMA_NFL',
#'   predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#'   covariates=c('AGE_bl','GENDER','EDUCAT')
#' )
#'
#' my_model <- my_stat$fit_fn(
#'   formula = my_formula,
#'   data = adni_sample
#' )
aba_ancova <- function() {
  fns <- list(
    'formula_fn' = aba_formula_std,
    'fit_fn' = aba_fit_ancova
  )
  fns$stat_type <- 'ancova'
  class(fns) <- 'abaStat'
  return(fns)
}

# fit a lm model
aba_fit_ancova <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

#' @export
aba_glance.ancova <- function(x, ...) {
  # tidy glance
  glance_df <- broom::glance(x)
  return(glance_df)
}



