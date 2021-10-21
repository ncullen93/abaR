

#' Create a glm stat to use for an aba model.
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
#' my_stat <- aba_glm()
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
aba_glm <- function() {
  fns <- list(
    'formula_fn' = aba_glm_formula,
    'fit_fn' = aba_glm_fit,
    'evaluate_fn' = aba_glm_evaluate
  )
  return(fns)
}

# create formula for glm model
aba_glm_formula <- function(outcome, predictors, covariates) {
  f <- paste0(outcome, " ~ ")
  if (length(covariates) > 0) {
    f <- paste0(f, paste(covariates, collapse = " + "))
    if (length(predictors) > 0) f <- paste0(f, ' + ')
  }
  if (length(predictors) > 0) f <- paste0(f, paste(predictors, collapse = " + "))
  if (length(covariates) + length(predictors) == 0) f <- paste0(f, '1')
  return(f)
}

# fit a glm model
aba_glm_fit <- function(formula, data) {
  model <- stats::glm(stats::formula(formula), family = 'binomial', data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# evaluate a glm model
aba_glm_evaluate <- function(model, data) {

}


