

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
  fns$stat_type <- 'lme'
  class(fns) <- 'abaStat'

  return(fns)
}

aba_lme_formula <- function(outcome, predictors, covariates, ...) {
  formula_args <- list(...)
  time_var <- formula_args$time_var
  id_var <- formula_args$id_var
  interaction_vars <- formula_args$interaction_vars
  covariates <- covariates[!(covariates %in% interaction_vars)]

  f <- paste(outcome, "~", time_var)
  if (length(covariates) + length(predictors) > 0) f <- paste(f, '+')
  if (length(covariates) > 0) {
    f <- paste(f, paste(covariates, collapse = " + "))
    if (length(interaction_vars) > 0) {
      f <- paste(f, '+', paste0(interaction_vars, '*',
                                time_var, collapse=' + '))
    }
    if (length(predictors) > 0) f <- paste(f, '+')
  }
  if (length(predictors) > 0) f <- paste(f, paste0(predictors, "*",
                                                  time_var,
                                                  collapse = " + "))
  return(f)
}

# fit a lme model
aba_lme_fit <- function(formula, data, ...) {
  fit_args <- list(...)
  time_var <- fit_args$time_var
  id_var <- fit_args$id_var
  random_formula <- glue::glue('~ {time_var} | {id_var}')

  model <- nlme::lme(stats::formula(formula),
                     random = stats::formula(random_formula),
                     control = nlme::lmeControl(
                       maxIter = 1e10,
                       msMaxIter=1000,
                       opt = "optim"
                     ),
                     data = data, method = "ML")

  model$call$fixed <- stats::formula(formula)
  model$call$random <- stats::formula(random_formula)
  return(model)
}

# evaluate a lme model
aba_lme_evaluate <- function(model, data) {

}


