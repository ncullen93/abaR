#' Create a summary of an aba model.
#'
#' @param model abaModel. the model to create a summary from
#' @param ... other params
#'
#' @return abaSummary object
#' @export
#'
#' @examples
#' m <- aba_model()
aba_summary <- function(model, ...) {

  # coefficients and model metrics
  coefs_df <- coefs_summary(model)
  metrics_df <- metrics_summary(model)

  results_df <- coefs_df %>%
    bind_cols(metrics_df %>% select(-c(MID, groups, outcomes, stats)))

  s <- list(
    model = model,
    results = results_df
  )

  # run emmeans and pairs  if there is a treatment
  if (!is.null(model$spec$treatment)) {
    treatment_dfs <- treatment_summary(model)
    s$results_treatment <- treatment_dfs
  }

  class(s) <- 'abaSummary'
  return(s)
}

treatment_summary <- function(model) {
  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  treatment_var <- model$spec$treatment

  r <- model$results %>%
    filter(predictors != '') %>%
    rowwise() %>%
    mutate(
      stats_emmeans = list(
        aba_emmeans(
          fit = .data$stats_fit,
          treatment = .data$predictors,
          stats_obj = .data$stats_obj
        )
      )
    ) %>%
    unnest_wider(stats_emmeans)

  # separate into two different dfs
  emmeans_df <- r %>%
    select(MID:covariates, emmeans) %>%
    unnest(emmeans)

  pairs_df <- r %>%
    select(MID:covariates, pairs) %>%
    unnest(pairs)

  return(
    list(
      'emmeans' = emmeans_df,
      'pairs' = pairs_df
    )
  )
}


coefs_summary <- function(model) {

  coef_fmt <- function(est, lo, hi, pval) {
    coef <- glue('{sprintf("%.2f", est)}')
    if (!is.na(lo)) {
      coef <- glue('{coef} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
    }
    if (!is.na(p.value)) {
      coef <- glue('{coef} (P={sprintf("%.4f", pval)})')
    }
    coef
  }

  all_predictors <- model$spec$predictors %>%
    purrr::map(~strsplit(.,' | ',fixed=T)) %>%
    unlist() %>% unique()

  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  all_variables <- c(all_covariates, all_predictors)

  # coefficients
  r <- model$results %>% rowwise() %>%
    mutate(
      stats_coefs = list(
        aba_tidy(.data$stats_fit, all_predictors, all_covariates)
      )
    ) %>%
    unnest(
      .data$stats_coefs
    ) %>%
    select(
      -c(.data$predictors, .data$covariates),
      -c(.data$std.error, .data$statistic, stats_obj, stats_fit)
    )

  r <- r %>%
    rename(
      est = estimate,
      lo = conf.low,
      hi = conf.high,
      pval = p.value
    ) %>%
    pivot_wider(
      names_from = .data$term,
      values_from = c(.data$est, .data$lo, .data$hi, .data$pval),
      names_glue = '{term}_{.value}'
    ) %>%
    rename_with(
      ~stringr::str_replace(., '_est',''),
      ends_with('_est')
    ) %>%
    select(
      MID:stats,
      any_of(
        c(outer(c('','_lo','_hi', '_pval'), all_variables, FUN=function(x,y) paste0(y,x)))
      )
    )

  return(r)
}

metrics_summary <- function(model) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    'Cut',
    'AIC',
    'Pval',
    'nobs'
  )
  metric_fmt <- function(est, lo, hi) {
    metric <- glue('{sprintf("%.2f", est)}')
    if (!is.na(lo)) {
      metric <- glue('{metric} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
    }
    metric
  }

  # add null model
  model_results <- model$results %>%
    group_by(groups, outcomes, stats) %>%
    mutate(
      stats_fit_null = list(.data$stats_fit[.data$MID=='M1'])[[1]]
    ) %>% ungroup()

  r <- model_results %>% rowwise() %>%
    mutate(
      stats_metrics = list(
        aba_glance(
          x = .data$stats_fit,
          x0 = .data$stats_fit_null
        )
      )
    ) %>%
    unnest(
      .data$stats_metrics
    ) %>%
    select(
      -c(.data$predictors, .data$covariates),
      -c(.data$stats_obj, .data$stats_fit, .data$stats_fit_null)
    )

  r <- r %>%
    filter(
      term %in% metric_vars
    ) %>%
    rename(
      est = estimate,
      lo = conf.low,
      hi = conf.high
    ) %>%
    pivot_wider(
      names_from = .data$term,
      values_from = c(.data$est, .data$lo, .data$hi),
      names_glue = '{term}_{.value}'
    ) %>%
    select(
      where(~sum(!is.na(.))>0)
    ) %>%
    rename_with(
      ~stringr::str_replace(., '_est',''),
      ends_with('_est')
    ) %>%
    select(
      MID:stats,
      any_of(
        c(outer(c('','_lo','_hi'), metric_vars, FUN=function(x,y) paste0(y,x)))
      )
    )

  return(r)
}

#' @export
print.abaSummary <- function(x, ...) {

  r_nested <- x$results %>%
    group_by(
      .data$groups,
      .data$outcomes,
      .data$stats
    ) %>%
    nest() %>%
    mutate(
      label = glue('{groups} | {outcomes} | {stats}')
    )

  if (!is.null(x$model$spec$treatment)) {
    r2_nested <- x$results_treatment$emmeans %>%
      group_by(groups, outcomes, stats) %>% nest() %>%
      rename(emmeans = data)
    r3_nested <- x$results_treatment$pairs %>%
      group_by(groups, outcomes, stats) %>% nest() %>%
      rename(pairs = data)
    r_nested <- r_nested %>%
      left_join(r2_nested, by=c('groups','outcomes','stats')) %>%
      left_join(r3_nested, by=c('groups','outcomes','stats'))
  }


  r_split <- stats::setNames(
    split(r_nested, 1:nrow(r_nested)),
    r_nested$label
  )


  r_split %>% purrr::iwalk(
    function(x,y) {
      cat('\n------------------------------------\n')
      cat(y)
      cat('\n------------------------------------\n')
      cat('\n')
      cat('Coefficients & Metrics:\n\n')
      # coefficients
      print(
        x$data[[1]][,colMeans(is.na(x$data[[1]])) < 1]
      )
      if ('emmeans' %in% colnames(x)) {
        cat('\nTreatment effects:\n\n')
        print(
          x$pairs[[1]] %>%
            select(-c(predictors, covariates, term, null.value, df, statistic)) %>%
            rename(Comparison = contrast) %>%
            select(MID, Comparison, everything()) %>%
            rowwise() %>%
            mutate(
              estimate = as.character(
                glue('{sprintf("%.2f",estimate)} [{sprintf("%.2f",std.error)}] (P={sprintf("%.4f",p.value)})')
                )
            ) %>%
            select(-c(std.error, p.value, Comparison)) %>%
            pivot_wider(names_from = WEEK, values_from = estimate,
                        names_prefix = 'Estimate_Time_')
        )
      }
    }
  )
}
