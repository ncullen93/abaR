#' Create a cross validation evaluator
#'
#' @param nfolds integer. number of cv folds
#' @param ntrials integer. number of cv trials to run
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
#'   set_evals('cv') %>%
#'   fit()
eval_cv <- function(nfolds = 5, ntrials = 1) {
  struct <- list(
    nfolds = nfolds,
    ntrials = ntrials
  )
  struct$eval_type <- 'cv'
  class(struct) <- 'abaEval'
  struct
}

fit_cv <- function(object, nfolds = 5, ntrials = 1, verbose = FALSE) {

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

        # fold data
        fit_df <- fit_df %>%
          mutate(
            data = purrr::map(
              .data$data,
              function(data) {
                cv_idx <- sample(cut(1:nrow(data), breaks=nfolds, labels=F))
                data$cv_idx <- cv_idx
                data_list <- 1:nfolds %>% purrr::map(
                  function(idx) {
                    data_train <- data %>% dplyr::filter(.data$cv_idx != idx)
                    data_test <- data %>% dplyr::filter(.data$cv_idx == idx)
                    list('data' = data_train, 'data_test' = data_test)
                  }
                )
                names(data_list) <- 1:nfolds
                data_list
              }
            )
          ) %>%
          unnest_longer(data, indices_to = 'fold') %>%
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
          select(gid, oid, sid, pid, fit, .data$fold, .data$data_test) %>%
          rename(
            group = gid,
            outcome = oid,
            stat = sid,
            predictor = pid
          )

        fit_df <- fit_df %>%
          mutate(trial = index) %>%
          select(-c(fold, trial), everything())

        # check that all models are not null
        if (sum(purrr::map_lgl(fit_df$fit, ~!is.null(.))) == 0) {
          stop('All models failed to be fit. Check your model setup.')
        }
        fit_df
      }
    ) %>%
    bind_rows()

  fit_df <- fit_df %>% mutate(fold = as.integer(fold))

  model$results <- fit_df
  model$is_fit <- TRUE
  model$fit_type <- 'cv'
  return(model)
}

summary_cv <- function(model,
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

  results
}

