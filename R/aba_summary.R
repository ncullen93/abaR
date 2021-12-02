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
aba_summary <- function(model,
                        control = aba_control(),
                        verbose = FALSE,
                        ...) {

  # coefficients and model metrics
  coefs_df <- coefs_summary(model, control) %>% mutate(form = 'coef')
  metrics_df <- metrics_summary(model) %>% mutate(form = 'metric')
  results_df <- coefs_df %>% bind_rows(metrics_df) %>%
    select(group, outcome, stat, predictor_set, term, form, estimate:pval) %>%
    filter(!is.na(estimate))

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
          fit = .data$stat_fit,
          treatment = .data$predictors,
          stat_obj = .data$stat_obj
        )
      )
    ) %>%
    unnest_wider(stats_emmeans)

  # separate into two different dfs
  emmeans_df <- r %>%
    select(predictor_set:covariates, emmeans) %>%
    unnest(emmeans)

  pairs_df <- r %>%
    select(predictor_set:covariates, pairs) %>%
    unnest(pairs)

  return(
    list(
      'emmeans' = emmeans_df,
      'pairs' = pairs_df
    )
  )
}


coefs_summary <- function(model, control) {

  all_predictors <- model$spec$predictors %>%
    purrr::map(~strsplit(.,' | ',fixed=T)) %>%
    unlist() %>% unique()

  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

  # coefficients
  r <- model$results %>%
    rowwise() %>%
    mutate(
      stat_coefs = list(aba_tidy(.data$stat_fit, all_predictors, all_covariates))
    ) %>%
    ungroup() %>%
    unnest(
      .data$stat_coefs,
      names_repair = 'unique'
    ) %>%
    select(
      -c(.data$predictor, .data$covariate,
         .data$std.error, .data$statistic,
         .data$stat_obj, .data$stat_fit)
    ) %>%
    rename(
      conf_low = conf.low,
      conf_high = conf.high,
      pval = p.value
    ) %>%
    select(-pval, everything())

  ## remove intercept if specified in control
  #if (!control$include_intercept) {
  #  r <- r %>% filter(term != c('(Intercept)'))
  #}

  # remove covariates if specified in control
  if (!control$include_covariates) {
    r <- r %>% filter(!(term %in% c(all_covariates)))
  }

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
      estimate = as.character(coef_fmt(.data$estimate, .data$conf_low,
                                  .data$conf_high, .data$pval))
    ) %>%
    select(-c(pval, conf_low, conf_high)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$estimate
    )
}

metrics_summary <- function(model) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    #'Cut',
    'AIC',
    'Pval',
    'nobs',
    'nsub'
  )

  # add null model
  model_results <- model$results %>%
    group_by(group, outcome, stat) %>%
    mutate(
      stat_fit_null = list(
        ifelse(sum(c('M1','Basic') %in% .data$predictor_set)>0,
               .data$stat_fit[.data$predictor_set %in% c('M1','Basic')],
               NA)
      )[[1]]
    ) %>%
    ungroup()

  r <- model_results %>% rowwise() %>%
    mutate(
      stat_metrics = list(
        aba_glance(
          x = .data$stat_fit,
          x0 = .data$stat_fit_null
        )
      )
    )

  r <- r %>%
    unnest(
      .data$stat_metrics
    )

  r <- r %>%
    select(
      -c(.data$predictor, .data$covariate),
      -c(.data$stat_obj, .data$stat_fit, .data$stat_fit_null)
    )

  r <- r %>%
    filter(
      term %in% metric_vars
    ) %>%
    rename(
      estimate = estimate,
      conf_low = conf.low,
      conf_high = conf.high
    ) %>%
    arrange(
      match(term, metric_vars)
    )

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
      estimate = as.character(metric_fmt(.data$estimate, .data$conf_low,
                                    .data$conf_high))
    ) %>%
    select(-c(conf_low, conf_high)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$estimate
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
      .data$group,
      .data$outcome,
      .data$stat
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
  ) %>%
    select(
      -c(any_of('(Intercept)'))
    )

  r_metric <- metric_pivot_wider(
    x_res %>%
      filter(form == 'metric') %>%
      select(-c('form'))
  )

  r_metric <- r_metric %>%
    select(-nobs, everything())

  r_results <- r_coef %>%
    bind_cols(
      r_metric %>% select(-c(predictor_set, group, outcome, stat))
    )

  # replace group names for printing if they exist
  if (!is.null(names(x$model$spec$groups))) {
    r_results$group <- factor(
      r_results$group,
      levels = x$model$spec$groups,
      labels = names(x$model$spec$groups)
    )
  }

  # replace outcome names for printing if they exist
  if (!is.null(names(x$model$spec$outcomes))) {
    r_results$outcome <- factor(
      r_results$outcome,
      levels = x$model$spec$outcomes,
      labels = names(x$model$spec$outcomes)
    )
  }

  r_nested <- r_results %>%
    group_by(
      .data$group,
      .data$outcome,
      .data$stat
    ) %>%
    nest() %>%
    mutate(
      label = glue('{group} | {outcome} | {stat}')
    )

  if (!is.null(x$model$spec$treatment)) {
    r2_nested <- x$results_treatment$emmeans %>%
      group_by(group, outcome, stat) %>% nest() %>%
      rename(emmeans = data)
    r3_nested <- x$results_treatment$pairs %>%
      group_by(group, outcome, stat) %>% nest() %>%
      rename(pairs = data)
    r_nested <- r_nested %>%
      left_join(r2_nested, by=c('group','outcome','stat')) %>%
      left_join(r3_nested, by=c('group','outcome','stat'))
  }


  r_split <- stats::setNames(
    split(r_nested, 1:nrow(r_nested)),
    r_nested$label
  )

  r_length <- length(r_split)

  r_split %>% purrr::iwalk(
    function(x,y) {
      nchar_label <- nchar(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')
      cat(y)
      cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')

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
            select(-c(predictor, covariate, term, null.value, df, statistic)) %>%
            rename(comparison = contrast) %>%
            select(predictor_set, comparison, everything()) %>%
            rowwise() %>%
            mutate(
              estimate = as.character(
                glue('{sprintf("%.2f",estimate)} [{sprintf("%.2f",std.error)}] (P={sprintf("%.4f",p.value)})')
              )
            ) %>%
            select(-c(std.error, p.value, comparison)) %>%
            pivot_wider(names_from = WEEK, values_from = estimate,
                        names_prefix = 'Estimate_Time_')
        )
      }
    }
  )

  if (r_length < r_length_orig) {
    cat(glue('\n\n ... {r_length_orig - r_length} of',
    ' {r_length_orig} tables not printed ...\n\n\n'))
  }

}
