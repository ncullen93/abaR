

#' Create a lme stat to use for an aba model.
#'
#' @param id_var string or variable. id variable
#' @param time_var string or variable. time variable.
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
#' my_stat <- aba_lme(id_var='SUBJECT_ID',
#'                    time_var='Years_bl')
#'
#' #my_formula <- my_stat$formula_fn(
#' #  outcome='ConvertedToAlzheimers',
#' #  predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#' #  covariates=c('AGE_bl','GENDER','EDUCAT'),
#' #  id
#' #)
#'#
#' #my_model <- my_stat$fit_fn(
#' #  formula = my_formula,
#' #  data = adni_sample
#' #)
aba_lme <- function(id_var,
                    time_var) {
  fns <- list(
    'formula_fn' = aba_lme_formula,
    'fit_fn' = aba_lme_fit,
    'evaluate_fn' = aba_lme_evaluate,
    'extra_params' = list(
      'id_var' = id_var,
      'time_var' = time_var
    )
  )

  return(fns)
}

aba_lme_formula <- function() {

}

# fit a lme model
aba_lme_fit <- function(formula, data) {
  model <- nlme::lme(stats::formula(formula), family = 'binomial', data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# evaluate a lme model
aba_lme_evaluate <- function(model, data) {

}


