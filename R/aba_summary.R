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
  coef_fmt <- paste(
    '{sprintf("%.2f", estimate)}',
    '(P={sprintf("%.4f", p.value)})'
  )

  all_predictors <- model$spec$predictors %>%
    purrr::map(~strsplit(.,' | ',fixed=T)) %>%
    unlist() %>% unique()

  all_covariates <- model$spec$covariates
  if (is.null(all_covariates)) all_covariates <- c('')

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
    ) %>%
    mutate(
      coef = as.character(glue::glue(
        coef_fmt
      ))
    ) %>%
    select(-c(.data$estimate, .data$p.value)) %>%
    pivot_wider(
      names_from = .data$term,
      values_from = .data$coef
    ) %>%
    select(-'(Intercept)')

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
      .data$MID,
      .data$groups,
      .data$outcomes,
      .data$stats,
      any_of(metric_vars)
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

  if (!is.na(x$model$spec$treatment)) {
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
          x$emmeans[[1]]
        )
        print(
          x$pairs[[1]]
        )
      }
    }
  )
}
