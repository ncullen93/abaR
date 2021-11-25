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
  coefs_df <- coefs_summary(model) %>% mutate(form = 'coef')
  metrics_df <- metrics_summary(model) %>% mutate(form = 'metric')
  results_df <- coefs_df %>% bind_rows(metrics_df) %>%
    select(MID:term, form, everything())

  s <- list(
    model = model,
    results = results_df
  )

  # run emmeans and pairs  if there is a treatment
  if (!is.null(model$spec$treatment)) {
    treatment_dfs <- treatment_summary(model)
    s$results_treatments <- treatment_dfs
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

  all_predictors <- model$spec$predictors %>%
    purrr::map(~strsplit(.,' | ',fixed=T)) %>%
    unlist() %>% unique()

  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  all_vars <- c(all_covariates, all_predictors)

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
    select(-pval, everything())

  #r <- coef_pivot_wider(r)

  return(r)
}

coef_fmt <- function(est, lo, hi, pval) {
  coef <- glue('{sprintf("%.2f", est)}')
  if (!is.na(lo)) {
    coef <- glue('{coef} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
  }
  if (!is.na(pval)) {
    coef <- glue('{coef} (P={sprintf("%.4f", pval)})')
  }
  coef
}

coef_pivot_wider <- function(r) {
  r %>% rowwise() %>%
    mutate(
      est = as.character(coef_fmt(.data$est, .data$lo, .data$hi, .data$pval))
    ) %>%
    select(-c(pval, lo, hi)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$est
    )
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
    )

  #r <- metric_pivot_wider(r)

  return(r)
}

metric_fmt <- function(est, lo, hi) {
  metric <- glue('{sprintf("%.2f", est)}')
  if (!is.na(lo)) {
    metric <- glue('{metric} [{sprintf("%.2f", lo)}, {sprintf("%.2f", hi)}]')
  }
  metric
}

# convert to wide
metric_pivot_wider <- function(r) {
  r %>% rowwise() %>%
    mutate(
      est = as.character(metric_fmt(.data$est, .data$lo, .data$hi))
    ) %>%
    select(-c(lo, hi)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$est
    )
}

#' @export
print.abaSummary <- function(x, ...) {
  args <- list(...)
  nprint <- 5
  if ('n' %in% names(args)) nprint <- args$n
  if (nprint == -1) nprint <- 1000

  x_res <- x$results %>%
    group_by(
      .data$groups,
      .data$outcomes,
      .data$stats
    ) %>%
    nest()

  r_length_orig <- x_res %>% nrow()

  x_res <- x_res %>%
    ungroup() %>%
    slice(1:nprint) %>%
    unnest(cols=c(.data$data))

  r_coef <- coef_pivot_wider(
    x_res %>%
      filter(form == 'coef') %>%
      select(-c('form'))
  ) %>% select(-c('(Intercept)'))
  r_metric <- metric_pivot_wider(
    x_res %>%
      filter(form == 'metric') %>%
      select(-c('form'))
  )
  r_results <- r_coef %>%
    bind_cols(
      r_metric %>% select(-c(MID, groups, outcomes, stats))
    )

  r_nested <- r_results %>%
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

  r_length <- length(r_split)

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

  if (r_length < r_length_orig) {
    cat(glue('\n\n ... {r_length_orig - r_length} of',
    ' {r_length_orig} tables not printed ...'))
  }

}
