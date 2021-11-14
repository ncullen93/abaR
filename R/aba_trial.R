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
                      spec = aba_trial_spec(),
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


#' Compile an abaTrial model
#'
#' @param model abaTrial. model to compile
#'
#' @return abaTrial
#' @export
#'
#' @examples
#' m <- aba_trial()
compile.abaTrial <- function(model) {

  data <- model$data
  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  time_var <- model$spec$time_var
  time_vals <- model$spec$times
  stat_vals <- model$spec$stats

  val_list <- list(
    'groups' = group_vals,
    'outcomes' = as.vector(outcome_vals),
    'times' = as.vector(time_vals),
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
        time=.data$times,
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
  data, group, outcome, time_var, time, stats
) {

  # filter original data by group
  my_data <- data %>% dplyr::filter(
    rlang::eval_tidy(rlang::parse_expr(group))
  )

  # fit all of the stats on the given parameters
  stat_models <- stats %>%
    purrr::map(
      function(stat_obj) {
        my_formula <- stat_obj$formula_fn(
          outcome,
          time_var,
          time
        )
        my_model <- stat_obj$fit_fn(my_formula, my_data)
        return(my_model)
      }
    )

  return(
    list(stat_models)
  )
}

#' @export
print.abaTrial <- function(x, ...) {
  model <- x

  group_vals <- model$spec$group
  endpoint_vals <- model$spec$endpoints
  treatment_vals <- model$spec$treatment
  covariate_vals <- model$spec$covariates
  stat_vals <- model$spec$stats
  data <- model$data
#
  cat('Treatment:\n   ')
  cat(treatment_vals, sep='\n   ')
  cat('Groups:\n   ')
  cat(group_vals, sep='\n   ')
  cat('Endpoints:\n   ')
  cat(endpoint_vals, sep='\n   ')
  cat('Covariates:\n   ')
  cat(covariate_vals, sep=', ')
  cat('\n')
  cat('Stats:\n   ')
  for (stat_idx in seq_along(stat_vals)) {
    stat_val <- stat_vals[[stat_idx]]
    x <- stat_val
    cat(x$stat_type)
    if (!is.null(x$extra_params)) {
      cat('(')
      ep <- x$extra_params
      for (ix in seq_along(ep)) {
        cat(names(ep)[ix], ' = ', ep[[ix]], sep='')
        if (ix != length(ep)) cat(' | ')
      }
      cat(')')
    }
    if (stat_idx != length(stat_vals)) cat('\n   ')
    #else cat('\n')
  }

  #xx <- stat_vals %>% purrr::walk(~print(.$stat_type))
  cat('\n')
  cat('Data:\n   ')
  if (!is.null(data)) {
    cat(paste(nrow(model$data), 'x', ncol(model$data)), sep='\n   ')
  }
}





