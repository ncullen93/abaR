#' Get individual predictions from a fitted aba model
#'
#' This function allows you to get the fitted/predicted values from data
#' used to fit an aba model or from new data. This is essentially an extension
#' of the `broom::augment()` function.
#'
#' @param object a fitted aba model. The model to get predictions from.
#' @param ... additional parameters. Accepted: newdata, merge, augment. See
#'   `aba_fit()` for more information.
#'
#' @return a dataframe with original data and with fitted/predicted values added
#'   for each group-outcome-stat-predictor combination.
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CDRSB_bl, MMSE_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('lm')
#'
#' model <- model %>% fit()
#'
#' # add model predictions to original data
#' data_augmented <- model %>% aba_predict()
#'
#' # store predictions separately by group - outcome - stat combination
#' data_augmented2 <- model %>% aba_predict(merge = FALSE)
predict.abaModel <- function(object, ...) {
  aba_predict(object, ...)
}


#' Get individual predictions from a fitted aba model
#'
#' This function allows you to get the fitted/predicted values from data
#' used to fit an aba model or from new data. This is essentially an extension
#' of the `broom::augment()` function.
#'
#' @param model a fitted aba model. The model to get predictions from.
#' @param newdata dataframe (optional). New data to get predictions from. If
#'   this is null, predictions will be provided for the data originally used
#'   to fit the aba model
#' @param merge logical. Whether to merge all the predictions into the original
#'   dataset or to store the predictions for each group - outcome - stat combo
#'   in a dataframe separately.
#' @param augment logical. Whether to include original data with predictions.
#'
#' @return a dataframe with original data and with fitted/predicted values added
#'   for each group-outcome-stat-predictor combination.
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CDRSB_bl, MMSE_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats('lm')
#'
#' model <- model %>% fit()
#'
#' # add model predictions to original data
#' data_augmented <- model %>% aba_predict()
#'
#' # store predictions separately by group - outcome - stat combination
#' data_augmented2 <- model %>% aba_predict(merge = FALSE)
aba_predict <- function(
  model, newdata = NULL, merge = TRUE, augment = FALSE
) {
  results <- model$results
  eval_type <- model$evals[[1]]$eval_type

  # if newdata is given, process it and add to results
  if (!is.null(newdata)) {
    # mark newdata with row index for merging later
    newdata$.row_idx <- 1:nrow(newdata)

    results <- results %>%
      group_by(group, outcome, stat) %>%
      nest() %>%
      rename(info=data) %>%
      rowwise() %>%
      mutate(
        data_test = process_dataset(
          data = newdata,
          group = model$groups[[group]],
          outcome = model$outcomes[[outcome]],
          stat = model$stats[[stat]],
          predictors = model$predictors,
          covariates = model$covariates
        )
      ) %>%
      ungroup()

    # get row index for merging later
    main_index <- results %>%
      mutate(.row_idx = purrr::map(.data$data_test, '.row_idx')) %>%
      select(-c(info, data_test))

    main_data <- newdata
    results <- results %>% unnest(info)

    if (!merge) newdata$.row_idx <- NULL

  } else {
    results <- results %>% mutate(data_test = list(NULL))
    main_index <- model$index
    main_data <- model$data

    if (eval_type == 'standard') {
      results$trial <- list(NULL)
    }
  }

  # get model predictions on data
  results <- results %>%
    rowwise() %>%
    mutate(
      data = augment_helper(
        fit, group, outcome, stat, predictor, data_test, augment, merge, trial
      )
    ) %>%
    select(-c(data_test, fit)) %>%
    ungroup()

  if (eval_type == 'standard') results <- results %>% select(-trial)

  # merge all predictions with original dataset
  if (merge) {
    if (eval_type == 'standard') {
      results <- results %>% group_by(group, outcome, stat)
    } else if (eval_type == 'boot') {
      results <- results %>% group_by(group, outcome, stat, trial)
    }

    results <- results %>%
      nest() %>%
      mutate(
        data = purrr::map(
          data, ~merge_datasets(.$data)
        )
      ) %>%
      ungroup()

    # add in row index to each augmented data
    results <- results %>%
      left_join(main_index, by = c('group','outcome','stat'))

    results <- results %>%
      select(-c(group,outcome,stat)) %>%
      mutate(
        data = purrr::pmap(
          list(data, .row_idx),
          function(data, idx) {
            data <- data %>% select(contains('.fitted'))
            data$.row_idx <- idx
            data
          }
        )
      )

    results_df <- results$data %>%
      reduce(left_join, by = '.row_idx') %>%
      select(.row_idx, everything())

    # add back into original data
    results <- main_data %>%
      left_join(results_df, by = '.row_idx') %>%
      select(-.row_idx)

    if (!augment) results <- results %>% select(contains('.fitted'))
  }

  results
}

augment_helper <- function(
  fit, group, outcome, stat, predictor, newdata, augment, merge, trial
) {

  df <- broom::augment(fit, newdata=newdata) %>%
    select(1:all_of('.fitted'))

  if (!augment) df <- df %>% select(.fitted)

  if (merge) {
    fit_label <- glue('.fitted__{group}__{outcome}__{stat}__{predictor}')
    if (!is.null(trial)) fit_label <- glue('{fit_label}__{trial}')
    df <- df %>% rename({{ fit_label }} := .fitted)
  }

  list(df)
}

merge_datasets <- function(data_list, include_data) {
  data <- data_list[[1]]
  for (idx in seq_along(data_list)) {
    if (idx > 1) {
      data <- data %>%
        bind_cols(
          data_list[[idx]] %>% select(-any_of(colnames(data)))
        )
    }
  }
  data <- data %>% select(-contains('.fitted'), everything())
  data
}
