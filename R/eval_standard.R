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

fit_standard <- function(model, verbose = FALSE) {
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
    ungroup()

  index <- fit_df %>%
    mutate(
      group = purrr::map_chr(.data$info, ~.[['gid']][1]),
      outcome = purrr::map_chr(.data$info, ~.[['oid']][1]),
      stat = purrr::map_chr(.data$info, ~.[['sid']][1]),
      .row_idx = purrr::map(.data$data, '.row_idx')
    ) %>%
    select(-c(info, data))

  fit_df <- fit_df %>% unnest(info)
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
  model$index <- index
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


# helper function for aba_summary
as_table_coefs <- function(results, control = aba_control()) {
  df <- results %>% select(-any_of(c('bias')))

  # handle digits
  df <- df %>%
    mutate(
      across(estimate:conf_high,
             ~purrr::map_chr(., ~sprintf(glue('%.{control$coef_digits}f'), .))),
      pval = purrr::map_chr(
        pval,
        ~clip_metric(
          sprintf(glue('%.{control$pval_digits}f'), .),
          control$pval_digits
        )
      )
    )

  df <- df %>%
    mutate(
      estimate = purrr::pmap_chr(
        list(
          est = .data$estimate,
          lo = .data$conf_low,
          hi = .data$conf_high,
          pval = .data$pval
        ),
        coef_fmt
      )
    ) %>%
    select(-c(conf_low, conf_high, pval)) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )

  df
}

# helper function for aba summary
coef_fmt <- function(est, lo, hi, pval) {
  coef <- glue('{est}')
  if (!is.na(lo) | !is.na(hi)) coef <- glue('{coef} [{lo}, {hi}]')
  if (!is.na(pval)) {
    if (grepl('<',pval)) {
      coef <- glue('{coef} (P{pval})')
    } else {
      coef <- glue('{coef} (P={pval})')
    }
  }
  coef
}

clip_metric <- function(metric, digits) {
  if (is.na(metric)) return(metric)
  if (metric == paste0('0.',paste0(rep('0', digits),collapse=''))) {
    metric <- paste0('<0.',paste0(rep('0', digits-1),collapse=''),'1')
  }
  metric
}

# default digits that will never change
default_digits_map <- list(
  'nobs' = 0,
  'nsub' = 0,
  'nevent' = 0
)

# helper function for aba summary
metric_fmt <- function(est, lo, hi, term, control) {

  if (term %in% names(default_digits_map)) {
    digits <- default_digits_map[[term]]
  } else if (glue('{term}_digits') %in% names(control)) {
    digits <- control[[glue('{term}_digits')]]
  } else {
    digits <- control$metric_digits
  }
  fmt <- glue('%.{digits}f')

  metric <- glue('{sprintf({fmt}, est)}')
  if (!is.na(lo)) {
    metric <- glue('{metric} [{sprintf({fmt}, lo)}, {sprintf({fmt}, hi)}]')
  }

  metric
}

as_table_metrics <- function(results, control = aba_control()) {
  df <- results %>% select(-any_of(c('bias')))
  df <- df %>%
    mutate(
      estimate = purrr::pmap_chr(
        list(
          est = .data$estimate,
          lo = .data$conf_low,
          hi = .data$conf_high,
          term = .data$term
        ),
        metric_fmt,
        control = control
      )
    ) %>%
    select(-c(conf_low, conf_high)) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )

  if ('pval' %in% colnames(df)) {
    df <- df %>% mutate(
      pval = purrr::map_chr(pval, clip_metric, control$pval_digits)
    )
  }

  df
}

as_table_standard <- function(results, control) {
  coefs <- results$coefs %>% as_table_coefs(control)
  metrics <- results$metrics %>% as_table_metrics(control)
  tbl <- coefs %>%
    left_join(metrics, by=c('group','outcome','stat','predictor')) %>%
    select(all_of(colnames(coefs)),everything())
  list(
    'coefs_metrics' = tbl
  )
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
