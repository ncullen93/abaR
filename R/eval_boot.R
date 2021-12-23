#' Create a bootstrap evaluator
#'
#' @param ntrials integer. number of trials to perform
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
#'   set_evals(eval_boot(ntrials = 5)) %>%
#'   fit()
eval_boot <- function(ntrials = 10) {
  struct <- list(
    ntrials = ntrials
  )
  struct$eval_type <- 'boot'
  class(struct) <- 'abaEval'

  struct
}


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
          ungroup()

        # apply bootstrap sampling to data
        fit_df <- fit_df %>%
          mutate(
            data = purrr::map(
              .data$data,
              function(data) {
                data[sample(1:nrow(data), nrow(data), replace=TRUE),]
              }
            )
          ) %>%
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

        fit_df <- fit_df %>% mutate(trial = index - 1)

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

summary_boot <- function(object,
                         label,
                         control = aba_control(),
                         adjust = aba_adjust(),
                         verbose = FALSE) {
  if (length(object$evals) > 1) object$results <- object$results[[label]]

  ## coefs ##
  coefs_df <- object %>% calculate_coefs(control)

  coefs_df0 <- coefs_df %>%
    filter(.data$trial==0) %>%
    select(-c(.data$trial, conf_low, conf_high))

  coefs_df1 <- coefs_df %>%
    filter(.data$trial != 0) %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      conf_low = quantile(estimate, 0.025),
      conf_high = quantile(estimate, 0.975),
      .groups = 'keep'
    ) %>%
    ungroup()

  coefs_df <- coefs_df0 %>%
    left_join(
      coefs_df1,
      by = c("group", "outcome", "stat", "predictor", "term")
    ) %>%
    select(-pval, everything())

  ## metrics ##
  metrics_df <- object %>% calculate_metrics(control)

  metrics_df0 <- metrics_df %>%
    filter(.data$trial==0) %>%
    select(-c(.data$trial, conf_low, conf_high))

  metrics_df1 <- metrics_df %>%
    filter(.data$trial != 0) %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      conf_low = quantile(estimate, 0.025),
      conf_high = quantile(estimate, 0.975),
      .groups = 'keep'
    ) %>%
    ungroup() %>%
    filter(
      !term %in% c('nobs', 'pval')
    )

  metrics_df <- metrics_df0 %>%
    left_join(
      metrics_df1,
      by = c("group", "outcome", "stat", "predictor", "term")
    )

  results = list(
    coefs = coefs_df,
    metrics = metrics_df
  )

  if (adjust$method != 'none') results <- adjust_pvals(results, adjust)

  results
}
