#' Create a standard evaluator
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
#'   set_evals('standard') %>%
#'   fit()
eval_standard <- function() {
  struct <- list()
  struct$eval_type <- 'standard'
  class(struct) <- 'abaEval'
  struct
}

fit_standard <- function(object, verbose = FALSE) {
  model <- object
  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$predictors)) model$predictors <- list('Basic'=c())
  if (is.null(model$evals)) model <- model %>% set_evals(eval_standard())

  # compile model
  fit_df <- model %>% aba_compile()

  # progress bar
  pb <- NULL
  if (verbose) pb <- progress::progress_bar$new(total = nrow(fit_df))

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

  # check that all models are not null
  if (sum(purrr::map_lgl(fit_df$fit, ~!is.null(.))) == 0) {
    stop('All models failed to be fit. Check your model setup.')
  }

  model$results <- fit_df
  model$is_fit <- TRUE
  model$fit_type <- 'standard'
  return(model)
}

summary_standard <- function(object,
                             label,
                             control = aba_control(),
                             adjust = aba_adjust(),
                             verbose = FALSE) {
  if (length(object$evals) > 1) object$results <- object$results[[label]]

  coefs_df <- object %>% calculate_coefs(control)
  metrics_df <- object %>% calculate_metrics(control)

  results = list(
    coefs = coefs_df,
    metrics = metrics_df
  )

  if (adjust$method != 'none') results <- adjust_pvals(results, adjust)

  results
}

#' @export
print.abaEval <- function(x, ...) {
  cat(x$eval_type)
  params <- names(x)
  eval_type <- x$eval_type
  params <- params[params != 'eval_type']
  if (length(params) > 0) {
    cat('(')
    for (param in params) {
      cat(param, ' = ', x[[param]], sep='')
      if (param != params[length(params)]) cat(' | ')
    }
    cat(')')
  }
}
