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
#' @param include_data logical. Whether to also include the data with the
#'   fitted/predicted values or just the fitted/predicted values for each fit.
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
#' data_augmented <- model %>% aba_predict()
aba_predict <- function(model, newdata = NULL, include_data = TRUE) {
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
        data, ~merge_datasets(.$data_augment, include_data)
      )
    ) %>%
    ungroup() %>%
    select(-data)

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
  if (!include_data) data <- data %>% select(contains('.fitted'))
  data
}
