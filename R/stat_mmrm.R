

#' Create a mmrm stat to use for an aba model.
#'
#' @param id string. id variable
#' @param time string. time variable
#'   (should be discrete visits common to all participants)
#' @param treatment string. treatment variable.
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
#' my_stat <- aba_mmrm(id='SUBJECT_ID',
#'                     time='Years_bl')
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
aba_mmrm <- function(id,
                     time,
                     treatment=NULL) {
  fns <- list(
    'formula_fn' = aba_formula_lme,
    'fit_fn' = aba_fit_mmrm,
    'extra_params' = list(
      'id' = id,
      'time' = time
    )
  )
  if (!is.null(treatment)) fns$extra_params$treatment <- treatment
  fns$stat_type <- 'mmrm'
  class(fns) <- 'abaStat'

  return(fns)
}

# fit a mmrm model
aba_fit_mmrm <- function(formula, data, extra_params) {
  time <- extra_params$time
  id <- extra_params$id
  correlation_form <- glue::glue('~ 1 | {id}')
  weights_form <- glue::glue('~ 1 | {time}')

  # make sure data is in the right format:
  # - time variable should be a factor
  # - first visit should be removed
  unique_visits <- levels(factor(data[[time]]))
  if (length(unique_visits) > 10) {
    stop('10+ unique visits detected... MMRM requires discrete time points!
         Did you accidently use a continuous time variable?')
  }
  first_visit <- unique_visits[1]
  data <- data %>% distinct() %>%
    filter(.data[[time]] != first_visit) %>%
    mutate({{ time }} := factor(.data[[time]]))

  model <- nlme::gls(
    stats::formula(formula),
    correlation = nlme::corSymm(form = stats::formula(correlation_form)),
    weights = nlme::varIdent(form = stats::formula(weights_form)),
    data = data,
    na.action = na.omit,
    method = 'REML'
  )
  model$call$model <- stats::formula(formula)
  model$call$correlation$form <- stats::formula(correlation_form)
  model$call$weights$form <- stats::formula(weights_form)
  #model$call$data <- data

  return(model)
}

#' @export
aba_glance.gls <- function(x, ...) {
  glance_df <- broom.mixed::glance(x) %>% select(-logLik)
  return(glance_df)
}

