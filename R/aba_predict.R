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
#' @param merged logical. Whether to merge all the predictions into the original
#'   dataset or to store the predictions for each group - outcome - stat combo
#'   in a dataframe separately.
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
#' data_augmented2 <- model %>% aba_predict(merged = FALSE)
aba_predict <- function(model, newdata = NULL, merged = TRUE) {
  results <- model$results

  # if newdata is given, process it and add to results
  if (!is.null(newdata)) {
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
      ungroup() %>%
      unnest(info)
  } else {
    results <- results %>% mutate(data_test = list(NULL))
  }

  # get model predictions on data
  results <- results %>%
    rowwise() %>%
    mutate(
      data_augment = augment_helper(
        fit, group, outcome, stat, predictor, data_test
      )
    ) %>%
    group_by(group, outcome, stat) %>%
    nest() %>%
    mutate(
      data_augment = purrr::map(
        data, ~merge_datasets(.$data_augment)
      )
    ) %>%
    ungroup() %>%
    select(-data)

  # merge all predictions with original dataset
  if (merged) {
    # add in row index to each augmented data
    results <- results %>%
      left_join(model$index, by = c('group','outcome','stat'))

    results <- results %>%
      select(-c(group,outcome,stat)) %>%
      mutate(
        data = purrr::pmap(
          list(data_augment, .row_idx),
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
    results <- model$data %>%
      left_join(results_df, by = '.row_idx') %>%
      select(-.row_idx)
  }

  results
}

augment_helper <- function(fit, group, outcome, stat, predictor, newdata) {
  fit_label <- glue('.fitted__{group}__{outcome}__{stat}__{predictor}')
  df <- broom::augment(fit, newdata=newdata) %>%
    rename({{ fit_label }} := .fitted)
  fit_idx <- which(colnames(df) == fit_label)
  df <- df %>% select(1:all_of(fit_idx))
  list(df)
}

merge_datasets <- function(data_list) {
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
