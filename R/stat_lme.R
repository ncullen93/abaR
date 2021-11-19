

#' Create a lme stat to use for an aba model.
#'
#' @param id string or variable. id variable
#' @param time string or variable. time variable.
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
#' my_stat <- aba_lme(id='SUBJECT_ID',
#'                    time='Years_bl')
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
aba_lme <- function(id,
                    time,
                    std.beta = FALSE,
                    complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = aba_formula_lme,
    'fit_fn' = aba_fit_lme,
    'extra_params' = list(
      'id' = id,
      'time' = time
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'lme'
  class(fns) <- 'abaStat'

  return(fns)
}

aba_formula_lme <- function(outcome, predictors, covariates, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  #interaction_vars <- extra_params$interaction_vars
  interaction_vars <- c()
  covariates <- covariates[!(covariates %in% interaction_vars)]

  f <- paste(outcome, "~", time)
  if (length(covariates) + length(predictors) > 0) f <- paste(f, '+')
  if (length(covariates) > 0) {
    f <- paste(f, paste(covariates, collapse = " + "))
    if (length(interaction_vars) > 0) {
      f <- paste(f, '+', paste0(interaction_vars, '*',
                                time, collapse=' + '))
    }
    if (length(predictors) > 0) f <- paste(f, '+')
  }
  if (length(predictors) > 0) f <- paste(f, paste0(predictors, "*",
                                                  time,
                                                  collapse = " + "))
  return(f)
}

# fit a lme model
aba_fit_lme <- function(formula, data, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  random_formula <- glue::glue('~ {time} | {id}')

  model <- nlme::lme(stats::formula(formula),
                     random = stats::formula(random_formula),
                     control = nlme::lmeControl(
                       maxIter = 1e10,
                       msMaxIter = 1000,
                       opt = "optim"
                     ),
                     na.action = stats::na.omit,
                     data = data, method = "REML")

  model$call$fixed <- stats::formula(formula)
  model$call$random <- stats::formula(random_formula)
  #model$call$data <- data
  return(model)
}

#' @export
aba_glance.lme <- function(x, ...) {

  glance_df <- broom.mixed::glance(x) %>% #select(-logLik)
    dplyr::bind_cols(
      tibble::tibble(
        R2 = suppressWarnings(MuMIn::r.squaredGLMM(x)[1,][['R2m']])
      )
    )

  return(glance_df)
}

