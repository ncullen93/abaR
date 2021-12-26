#' Create a train-test evaluator
#'
#' @param split double between 0 and 1. percent of data to use as train set
#' @param ntrials integer. number of train-test trials to run
#'
#' @return aba model
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm') %>%
#'   set_evals('traintest') %>%
#'   fit()
eval_traintest <- function(split = 0.8, ntrials = 1) {
  struct <- list(
    split = split,
    ntrials = ntrials
  )
  struct$eval_type <- 'traintest'
  class(struct) <- 'abaEval'
  struct
}

fit_traintest <- function(object, split = 0.8, ntrials = 1, verbose = FALSE) {

  model <- object
  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$predictors)) model$predictors <- list('Basic'=c())

  # compile model
  fit_df <- model %>% aba_compile()

  # progress bar
  pb <- NULL
  if (verbose) pb <- progress::progress_bar$new(total = nrow(fit_df))

  fit_df <- 1:ntrials %>%
    purrr::map(
      function(index) {
        fit_df <- fit_df %>%
          group_by(group, outcome, stat) %>%
          nest() %>%
          rename(info=data) %>%
          rowwise() %>%
          mutate(
            data = process_dataset(
              data = model$data,
              group = .data$group,
              outcome = .data$outcome,
              stat = .data$stat,
              predictors = model$predictors,
              covariates = model$covariates
            )
          ) %>%
          ungroup()

        fit_df <- fit_df %>%
          mutate(
            data = purrr::map(
              .data$data,
              function(data) {
                train_idx <- sample(1:nrow(data), split*nrow(data), replace=FALSE)
                test_idx <- setdiff(1:nrow(data), train_idx)
                data_train <- data[train_idx,]
                data_test <- data[test_idx,]
                list('data'=data_train, 'data_test'=data_test)
              }
            )
          ) %>%
          unnest_wider(data) %>%
          unnest(info)

        # fit model
        fit_df <- fit_df %>%
          rowwise() %>%
          mutate(
            fit = fit_stat(
              data = .data$data,
              outcome = .data$outcome,
              stat = .data$stat,
              predictors = .data$predictor,
              covariates = .data$covariate,
              pb = pb
            )
          ) %>%
          ungroup()

        # select only factor labels and fit
        fit_df <- fit_df %>%
          select(gid, oid, sid, pid, fit, .data$data_test) %>%
          rename(
            group = gid,
            outcome = oid,
            stat = sid,
            predictor = pid
          )

        fit_df <- fit_df %>% mutate(trial = index)

        # check that all models are not null
        if (sum(purrr::map_lgl(fit_df$fit, ~!is.null(.))) == 0) {
          stop('All models failed to be fit. Check your model setup.')
        }
        fit_df
      }
    ) %>%
    bind_rows()

  model$results <- fit_df
  model$is_fit <- TRUE
  model$fit_type <- 'traintest'
  return(model)
}

summary_traintest <- function(model,
                              label,
                              control = aba_control(),
                              adjust = aba_adjust(),
                              verbose = FALSE) {
  if (length(model$evals) > 1) model$results <- model$results[[label]]
  results <- model$results

  # grab stat object
  results <- results %>%
    mutate(
      stat_obj = purrr::map(stat, ~model$stats[[.]])
    )

  # use evaluate function from stat object on fitted model and test data
  results <- results %>%
    mutate(
      results_test = purrr::pmap(
        list(stat_obj, fit, data_test),
        function(stat_obj, fit, data_test) {
          x <- stat_obj$fns$evaluate(fit, data_test)
          x
        }
      )
    )

  results <- results %>%
    select(-c(fit, data_test, stat_obj)) %>%
    unnest(results_test)

  # summarise values
  results <- results %>%
    pivot_longer(rmse:mae) %>%
    group_by(group, outcome, stat, predictor, form, name) %>%
    summarise(
      error_mean = mean(value),
      error_sd = sd(value),
      .groups='keep'
    ) %>%
    ungroup()

  results
}
