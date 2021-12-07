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

  criteria_map <- list('aic'='AIC', 'pval'='Pval')
  criteria <- criteria_map[[match.arg(criteria)]]

  if ((criteria == 'AIC') & is.null(threshold)) threshold <- -2
  if ((criteria == 'Pval') & is.null(threshold)) threshold <- 0.1

  m <- list(
    'model' = model,
    'method' = method,
    'criteria' = criteria,
    'threshold' = threshold,
    'verbose' = verbose
  )
  class(m) <- 'abaSelection'

  m <- run_selection(m)
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
run_selection <- function(object,
                             ...) {

  model <- object$model
  # round 0 - best models are the basic models
  results <- model$results %>%
    group_by(group, outcome, stat) %>%
    filter(row_number() == 1L) %>%
    ungroup() %>%
    select(group:stat) %>%
    rowwise() %>%
    mutate(
      model_0 = list(
        create_new_model(
          model,
          .data$group,
          .data$outcome,
          .data$stat
        )
      )
    )
  results$model0_est <- results %>% ungroup() %>% pull(model_0) %>%
    purrr::map_dbl(
      function(m) {
        s <- aba_summary(m)$results %>%
          filter(predictor_set == 'M1', term == object$criteria)
        s$estimate
      }
    )

  n_predictors <- length(model$spec$predictors) - 1

  for (idx in 1:n_predictors) {
    if (object$verbose) cat('Round: ', idx, '\n')

    models_to_test <- results[[glue('model_{idx-1}')]] %>%
      map_lgl(~'abaModel' %in% class(.)) %>%
      sum()

    if (models_to_test > 0) {
      results <- results %>%
        mutate(
          'model_{idx}' := list(
            find_next_model(.data[[glue::glue('model_{idx-1}')]],
                            baseline_value = .data$model0_est,
                            criteria = object$criteria,
                            threshold = object$threshold,
                            verbose = object$verbose)
          )
        )

    } else {
      break
    }
  }

  results <- results %>%
    select(-model0_est) %>%
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
                            coef_pivot_wider() %>% filter(predictor_set=='M1') %>%
                            select(-c(group:form))),
      metric_summary = list(.data$value_summary$results %>% filter(form=='metric') %>%
                              metric_pivot_wider() %>% filter(predictor_set=='M1') %>%
                              select(-c(group:form, Pval, nobs)))
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
  model %>% aba_fit()
}

find_next_model <- function(object, baseline_value, criteria, threshold, verbose) {

  if ('abaModel' %in% class(object)) {
    # summarise
    object_summary <- object %>% aba_summary()

    # get best model
    best_model <- object_summary$results %>%
      filter(term == criteria) %>%
      group_by(group, outcome, stat) %>%
      mutate(
        est_diff = case_when(
          term == 'AIC' ~ estimate - min(baseline_value, first(estimate)),
          term == 'Pval' ~ estimate
        )
      ) %>%
      slice_min(est_diff, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-c(conf_low:pval))

    if (best_model$est_diff <= threshold) {

      # best predictors
      new_covariates <- object$results %>%
        filter(predictor_set == best_model$predictor_set) %>%
        pull(predictor)

      if (verbose) cat('Improvement: ', new_covariates, '\n')

      object$spec$predictors <- object$spec$predictors[
        object$spec$predictors != new_covariates
      ]

      object$spec$covariates <- c(
        object$spec$covariates,
        strsplit(new_covariates, ' + ', fixed=T)[[1]]
      )

      object <- object %>% aba_fit()

      return(object)
    } else {
      if (verbose) cat('No improvement - stopping\n')
      return(NA)
    }
  } else {
    if (verbose) cat('Found NA - skipping\n')
    return(NA)
  }
}


#' @export
print.abaSelection <- function(object, ...) {
  results <- object$results
  results <- results %>%
    group_by(group, outcome, stat) %>%
    nest() %>%
    mutate(
      label = glue('{group} | {outcome} | {stat}')
    )

  results_split <- stats::setNames(
    split(results, 1:nrow(results)),
    results$label
  )

  results_split %>% purrr::iwalk(
    function(info, label) {
      nchar_label <- nchar(label)
      cat('\n')
      cat(rep('-', nchar_label), sep='')
      cat('\n')
      cat(label)
      cat('\n')
      cat(rep('-', nchar_label), sep='')
      cat('\n')

      # reorder based on selection order
      info_data <- info$data[[1]]
      predictors <- colnames(info_data)[is.na(info_data[1,])]
      old_order <- purrr::map_int(predictors, ~sum(is.na(info_data[[.x]])))
      new_order <- sort(old_order, index.return=T)$ix
      predictors_new <- predictors[new_order]
      info_data[,predictors] <- info_data[,predictors_new]
      colnames(info_data)[colnames(info_data) %in% predictors] <- predictors_new

      print(info_data)
    }
  )
}







