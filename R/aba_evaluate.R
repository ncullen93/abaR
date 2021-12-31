#' Evaluate a fitted aba model on new data
#'
#' This function lets you evaluate the performance of a fitted aba model on
#' new data.
#'
#' @param model a fitted aba model. The model to test on new data
#' @param data dataframe. The new data on which the fitted model will be tested.
#'
#' @return an object with class 'abaEvaluate' which contains model performance
#'   on the data
#' @export
#'
#' @examples
#' # create separate training and testing datasets
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' train_id <- sample(nrow(data), 0.8*nrow(data), replace = FALSE)
#' data_train <- data %>% dplyr::filter(dplyr::row_number() %in% train_id)
#' data_test <- data %>% dplyr::filter(dplyr::row_number() %in% train_id)
#'
#' # fit model on train data
#' model <- aba_model() %>%
#'   set_data(data_train) %>%
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
#'   set_stats('lm') %>%
#'   fit()
#'
#' # evaluate fitted model on test data
#' res <- model %>% aba_evaluate(data = data_test)
aba_evaluate <- function(model, data) {
  tmp_model <- model
  newdata <- data %>% mutate(.row_idx = row_number())

  # add data to model results
  tmp_model$results <- tmp_model$results %>%
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
    unnest(info) %>%
    mutate(trial = 1)

  tmp_model <- tmp_model %>% set_evals(eval_traintest(ntrials=1, contrasts=F))
  tmp_model$is_fit <- TRUE
  tmp_model_summary <- tmp_model %>% summary()
  tmp_model_summary
}

#' @export
as_table.abaEvaluate <- function(object) {
  res <- object
  res$test_metrics <- res$test_metrics %>%
    mutate(conf_low = NA, conf_high = NA, estimate_train = NA)
  res_tbl <- as_table_traintest(results = res, control = aba_control())
  res_tbl
}




