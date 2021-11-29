#' Cfreate an aba selection object
#'
#' @param model model
#' @param method method
#' @param criteria criteria
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_selection <- function(model,
                          method = c('forward', 'backward'),
                          criteria = c('aic', 'pval'),
                          threshold = NULL,
                          verbose = FALSE) {

  method <- match.arg(method)
  criteria <- match.arg(criteria)

  if ((criteria == 'aic') & is.null(threshold)) threshold <- 2
  if ((criteria == 'pval') & is.null(threshold)) threshold <- 0.1

  m <- list(
    'model' = model,
    'method' = method,
    'criteria' = criteria,
    'threshold' = threshold,
    'verbose' = verbose
  )
  class(m) <- 'abaSelection'
  return(m)
}


#' Fit an aba selection object
#'
#' @param object object
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
fit.abaSelection <- function(object,
                             ...) {

  model <- object$model
  # round 0 - best models are the basic models
  results <- model$results %>%
    group_by(groups, outcomes, stats) %>%
    filter(row_number() == 1L) %>%
    ungroup() %>%
    select(groups:stats) %>%
    rowwise() %>%
    mutate(
      model_0 = list(
        create_new_model(
          model,
          .data$groups,
          .data$outcomes,
          .data$stats
        )
      )
    )

  for (idx in 1:10) {
    if (object$verbose) cat('Round: ', idx, '\n')
    models_to_test <- results[[glue('model_{idx-1}')]] %>%
      map_lgl(~'abaModel' %in% class(.)) %>%
      sum()

    if (models_to_test > 0) {
      results <- results %>%
        mutate(
          'model_{idx}' := list(
            find_next_model(.data[[glue::glue('model_{idx-1}')]],
                            verbose = object$verbose)
          )
        )
    } else {
      break
    }
  }

  results <- results %>%
    ungroup() %>%
    pivot_longer(contains('model_')) %>%
    rowwise() %>%
    filter(!is.na(value %>% map(1))[1]) %>%
    ungroup()

  results <- results %>%
    rowwise() %>%
    mutate(
      value_summary = list(aba_summary(.data$value)),
      coef_summary = list(.data$value_summary$results %>% filter(form=='coef') %>%
                            coef_pivot_wider() %>% filter(MID=='M1') %>%
                            select(-c(MID:form))),
      metric_summary = list(.data$value_summary$results %>% filter(form=='metric') %>%
                              metric_pivot_wider() %>% filter(MID=='M1') %>%
                              select(-c(MID:form, Pval, nobs)))
    )

  results <- results %>%
    unnest_wider(coef_summary) %>%
    unnest_wider(metric_summary) %>%
    select(where(~sum(!is.na(.x))>0), -any_of('(Intercept)'), -value) %>%
    select(-c(name, value_summary))

  object$results <- results
  return(object)
}

create_new_model <- function(model, group, outcome, stat) {
  model$spec$groups <- group
  model$spec$outcomes <- outcome
  model$spec$stats <- list(model$spec$stats[[stat]]) %>% set_names(stat)
  model %>% fit()
}

find_next_model <- function(object, verbose) {
  if ('abaModel' %in% class(object)) {
    # summarise
    object_summary <- object %>% aba_summary()

    # get best model
    best_model <- object_summary$results %>%
      filter(term == 'AIC') %>%
      group_by(groups, outcomes, stats) %>%
      mutate(
        est_diff = est - first(est)
      ) %>%
      slice_min(est_diff) %>%
      ungroup() %>%
      select(-c(lo:pval))

    if (best_model$est_diff <= -2) {

      # best predictors
      new_covariates <- object$results %>%
        filter(MID == best_model$MID) %>%
        pull(predictors)

      if (verbose) cat('Improvement: ', new_covariates, '\n')

      object$spec$predictors <- object$spec$predictors[
        object$spec$predictors != new_covariates
      ]

      object$spec$covariates <- c(
        object$spec$covariates,
        strsplit(new_covariates, ' | ', fixed=T)[[1]]
      )

      object <- object %>% fit()
      return(object)
    } else {
      if (verbose) cat('No improvement - stopping\n')
      return(NA)
    }
  } else {
    if (object$verbose) cat('Found NA - skippingv')
    return(NA)
  }
}
