#' Run model selection on an aba model.
#'
#' This function allows you to run model selection on a fitted aba model. The
#' function supports both forward and backward selection algorithms, both AIC
#' and p-value as selection criteria, and arbitrary thresholds.
#'
#' Forward selection starts from covariates-only and tests the addition of all
#' predictor sets individually, then adds the predictor set which improves the
#' model criteria the most. Backward selection starts from the inclusion of
#' all covariates + predictor sets and tests the removal of all predictor sets
#' individually, then removes the predictor set which improves the model criteria
#' the most. If there are no predictor sets whose addition/removal results in
#' an improvement in the selected criteria by a value at least as good as the
#' selected threshold, then the selection stops and the current model is frozen.
#' Also, note that the model selection procedure is run separately for each
#' group - outcome - stat combination.
#'
#' @param model abaModel. The fitted aba model to run selection on.
#' @param method string. The selection algorithm to use (forward or backward).
#' @param criteria string. Which metric to use when selecting the next
#'   model (aic or pval).
#' @param threshold numeric. Which threshold to use for the selected metric
#'   (defaults to -2 for aic; defaults to 0.1 for pval).
#' @param verbose logical. Whether to print out results of each selection round.
#'
#' @return an abaSelection object which contains model summary information such
#'  as coefficients and metrics for each selection round across the different
#'  groups/outcomes/stats.
#' @export
#'
#' @examples
#'
#' df <- aba::adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # standard model selection
#' model <- df %>% aba_model() %>%
#'   set_outcomes(ConvertedToAlzheimers) %>%
#'   set_predictors(
#'     CDRSB_bl, ADAS13_bl, MMSE_bl,
#'     CSF_ABETA_bl, CSF_PTAU_bl, CSF_TAU_bl,
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     MRI_HIPP_bl,
#'     PET_ABETA_bl
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('glm') %>%
#'   aba_fit()
#'
#' model_summary <- model %>% aba_summary()
#'
#' # default selection - forward selection by AIC with threshold = -2
#' \donttest{
#' model_selection <- model %>% aba_selection(verbose=TRUE)
#' }
#' # selection with p-value and threshold = 0.1
#' \donttest{
#' model_selection <- model %>%
#'   aba_selection(criteria = 'pval', threshold=0.1, verbose=TRUE)
#' }
#' # selection by group
#' model2 <- model %>%
#'   set_predictors(
#'     c(CDRSB_bl,ADAS13_bl,MMSE_bl),
#'     c(CSF_ABETA_bl,CSF_PTAU_bl,CSF_TAU_bl),
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl),
#'     c(MRI_HIPP_bl),
#'     c(PET_ABETA_bl)
#'   ) %>%
#'   aba_fit()
#'
#' model_summary2 <- model2 %>% aba_summary()
#'
#' \donttest{
#' model_selection2 <- model2 %>%
#'   aba_selection(criteria='pval', threshold=0.1, verbose=TRUE)
#' }
#'
#' # add more outcomes
#' model3 <- model2 %>%
#'   set_outcomes(ConvertedToAlzheimers, ConvertedToDementia) %>%
#'   aba_fit()
#'
#' \donttest{
#' model_selection3 <- model3 %>%
#'   aba_selection(criteria='pval', threshold=0.1, verbose=TRUE)
#' }
#'
#' # add more groups
#' model4 <- model3 %>%
#'   set_groups(everyone(), DX_bl %in% c('MCI','AD')) %>%
#'   aba_fit()
#'
#' \donttest{
#' model_selection4 <- model4 %>%
#'   aba_selection(criteria='pval', threshold=0.1, verbose=TRUE)
#' }
#'
aba_selection <- function(model,
                          method = c('forward', 'backward'),
                          criteria = c('aic', 'pval'),
                          threshold = NULL,
                          verbose = FALSE) {
  method <- match.arg(method)
  criteria <- match.arg(criteria)

  if (is.null(threshold)) {
    threshold_map <- list(
      'aic' = -2,
      'pval' = 0.1
    )
    threshold <- threshold_map[[criteria]]
  }

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

# helper function for aba selection
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
  results$model0_est <- results %>%
    ungroup() %>%
    pull(model_0) %>%
    purrr::map_dbl(
      function(m) {
        s <- aba_summary(m)$results$metrics %>%
          dplyr::filter(
            predictor == 'Basic',
            term == object$criteria
          )
        s$estimate
      }
    )

  n_predictors <- length(model$predictors) - 1

  for (idx in 1:n_predictors) {

    models_to_test <- results[[glue('model_{idx-1}')]] %>%
      map_lgl(~'abaModel' %in% class(.)) %>%
      sum()

    if (models_to_test > 0) {
      if (object$verbose) cat('Round: ', idx, '\n')
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
      coef_summary = list(.data$value_summary %>%
                            coefs_pivot_wider() %>%
                            filter(predictor=='Basic') %>%
                            select(-c(group:predictor))),
      metric_summary = list(.data$value_summary %>%
                              metrics_pivot_wider() %>%
                              filter(predictor=='Basic') %>%
                              select(-c(group:predictor, pval, nobs)))
    )

  results <- results %>%
    unnest_wider(coef_summary) %>%
    unnest_wider(metric_summary) %>%
    select(where(~sum(!is.na(.x))>0), -any_of('(Intercept)'), -value) %>%
    select(-c(name, value_summary))

  object$results <- results
  return(object)
}

# helper function for aba selection
create_new_model <- function(model, group, outcome, stat) {
  model$groups <- model$groups[group]
  model$outcomes <- model$outcomes[outcome]

  model$stats <- list(model$stats[[stat]]) %>% set_names(stat)
  model %>% aba_fit()
}

# helper function for aba selection
find_next_model <- function(object, baseline_value, criteria, threshold, verbose) {

  if ('abaModel' %in% class(object)) {
    # summarise
    object_summary <- object %>% aba_summary()

    # get best model
    best_model <- object_summary$results$metrics %>%
      filter(term == criteria) %>%
      group_by(group, outcome, stat) %>%
      mutate(
        est_diff = case_when(
          term == 'aic' ~ estimate - min(baseline_value, first(estimate)),
          term == 'pval' ~ estimate
        )
      ) %>%
      slice_min(est_diff, n = 1, with_ties = FALSE) %>%
      ungroup()

    if (best_model$est_diff <= threshold) {

      new_covariates <- object$predictors[[best_model$predictor]]
      if (verbose) cat('Improvement: ', new_covariates, '\n')

      object$predictors[[best_model$predictor]] <- NULL
      object$covariates <- c(object$covariates, new_covariates)
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
print.abaSelection <- function(x, ...) {
  object <- x
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
      new_order <- sort(old_order, index.return=TRUE)$ix
      predictors_new <- predictors[new_order]
      info_data[,predictors] <- info_data[,predictors_new]
      colnames(info_data)[colnames(info_data) %in% predictors] <- predictors_new

      print(info_data)
    }
  )
}

