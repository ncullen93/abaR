
#' Fit an aba model using bootstrap sampling
#'
#' This function fits an aba model first on the full dataset and then
#' using bootrstrap sampling.
#'
#' @param object aba model. The model to fit.
#' @param ntrials integer. Number of bootstrap trials to run.
#' @param verbose logical. Whether to print updates using progress bar.
#'
#' @return a fitted aba model
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model_spec <- aba_model() %>%
#'   set_data(data) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' model <- model_spec %>% fit_boot(ntrials = 5)
fit_boot <- function(object, ntrials, verbose = FALSE) {
  if (ntrials <= 1) stop('ntrials must be greater than 1.')
  model <- object
  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$predictors)) model$predictors <- list('Basic'=c())

  # compile model
  fit_df <- model %>% aba_compile()
  ntrials <- 1 + ntrials

  # progress bar
  pb <- NULL
  if (verbose) pb <- progress::progress_bar$new(total = ntrials*nrow(fit_df))

  fit_df <- 1:ntrials %>%
    purrr::map(
      function(index) {
        # add data
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
          ungroup() %>%
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
              is_boot = index != 1,
              pb = pb
            )
          ) %>%
          ungroup()

        # select only factor labels and fit
        fit_df <- fit_df %>%
          select(gid, oid, sid, pid, fit) %>%
          rename(
            group = gid,
            outcome = oid,
            stat = sid,
            predictor = pid
          )

        #if (ntrials > 0) {
        fit_df <- fit_df %>% mutate(boot_index = index - 1)
        #}

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
  model$fit_type <- 'boot'
  return(model)
}
