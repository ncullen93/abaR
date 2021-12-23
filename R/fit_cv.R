#' Fit an aba model using cross validation sampling
#'
#' This function fits an aba model on cross validation training data splits and
#' stores the test data for evaluation.
#'
#' @param object aba model. The model to fit.
#' @param nsplits integer. Number of equal-sized cross validation splits to use.
#' @param ntrials integer. Number of trials to run.
#' @param verbose logical. Whether to print updates using progress bar.
#'
#' @return a fitted aba model
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' model <- model %>% fit_cv(nsplits = 3, ntrials = 5)
fit_cv <- function(object, nsplits = 5, ntrials = 1, verbose = FALSE) {

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

        # split data
        fit_df <- fit_df %>%
          mutate(
            data = purrr::map(
              .data$data,
              function(data) {
                cv_idx <- sample(cut(1:nrow(data), breaks=nsplits, labels=F))
                data$cv_idx <- cv_idx
                data_list <- 1:nsplits %>% purrr::map(
                  function(idx) {
                    data_train <- data %>% dplyr::filter(.data$cv_idx != idx)
                    data_test <- data %>% dplyr::filter(.data$cv_idx == idx)
                    list('data' = data_train, 'data_test' = data_test)
                  }
                )
                names(data_list) <- paste0('split', 1:nsplits)
                data_list
              }
            )
          ) %>%
          unnest_longer(data, indices_to = 'split') %>%
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
          select(gid, oid, sid, pid, fit, .data$split, .data$data_test) %>%
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
  model$fit_type <- 'cv'
  return(model)
}


