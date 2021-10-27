#' Create an aba trial
#'
#' An aba trial is composed of the following:
#'   - data: a data.frame to be used to fit the trial models
#'   - spec: the specification for the aba trial
#'   - fits: the fitted statistical models once `fit()` is called
#'
#' @param data data.frame. the data to use for the object
#' @param spec trialSpec. the spec to use for the model. Can be created with
#'   trial_spec()
#' @param results list the fitted statistical models
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model()
aba_trial <- function(data = NULL,
                      spec = trial_spec(),
                      results = list()) {

  m <- list(
    'data' = data,
    'spec' = spec,
    'results' = results
  )

  class(m) <- 'abaTrial'

  return(
    m
  )
}

# compile abaTrial
#' @export
compile.abaTrial <- function(model) {

  data <- model$data
  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  time_var <- model$spec$time_var
  timepoint_vals <- model$spec$timepoints
  stat_vals <- model$spec$stats

  val_list <- list(
    'groups' = group_vals,
    'outcomes' = as.vector(outcome_vals),
    'time_var' = as.vector(time_var),
    'timepoints' = as.vector(timepoint_vals),
    'stats' = list(stat_vals)
  )

  init_df <- val_list %>% purrr::cross_df()
  init_df <- cbind(TID = stringr::str_c('T', rownames(init_df)), init_df)
  model$results <- init_df %>% dplyr::tibble()
  return(model)
}

#' Fit an aba trial
#'
#' This will trigger the fitting of all models
#' (`stats`) on the different parameter combinations (`spec`).
#'
#' @param object abaTrial The aba trial to be fitted.
#' @param ... additional parameters.
#'
#' @return abaTrial
#' @export
#' @examples
#' m <- aba_trial()
fit.abaTrial <- function(object, ...) {
  model <- object

  # compile model
  model <- model %>% compile()

  # fit stats on spec
  fit_df <- model$results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      fits = parse_then_fit_abaTrial(
        data=model$data,
        group=.data$groups,
        outcome=.data$outcomes,
        time_var=.data$time_VAR,
        timepoint=.data$timepoints,
        stats=.data$stats
      )
    ) %>%
    tidyr::unnest_wider(
      .data$fits
    )
#
  #model$results <- fit_df
  return(model)
}


# need a preprocessing function to parse
parse_then_fit_abaTrial <- function(
  data, group, outcome, time_var, timepoint, stats
) {

  # filter original data by group
  my_data <- data %>% dplyr::filter(
    rlang::eval_tidy(rlang::parse_expr(group))
  )

  # fit all of the stats on the given parameters
  stat_models <- stats %>%
    purrr::map(
      function(stat_obj) {
        my_formula <- stat_obj$formula_fn(outcome, predictors, covariates)
        my_model <- stat_obj$fit_fn(my_formula, my_data)
        return(my_model)
      }
    )

  return(
    list(stat_models)
  )
}

#print.abaTrial <- function(x, ...) {
#  model <- x
#
#  #group_vals <- model$spec$group
#  #outcome_vals <- model$spec$outcomes
#  #covariate_vals <- model$spec$covariates
#  #predictor_vals <- model$spec$predictors[-1]
#  #stat_vals <- model$spec$stats
##
#  #cat('Groups:\n   ')
#  #cat(group_vals, sep='\n   ')
#  #cat('Outcomes:\n   ')
#  #cat(outcome_vals, sep='\n   ')
#  #cat('Covariates:\n   ', covariate_vals, '\n')
#  #cat('Predictors:\n   ')
#  #cat(predictor_vals, sep='\n   ')
#  #cat('Stats:\n   ')
#  #xx <- stat_vals %>% purrr::map_chr(~.$stat_type)
#  #cat(xx, sep='\n   ')
#}





