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
  coefs_df <- coefs_summary(model)
  metrics_df <- metrics_summary(model)
  results_df <- coefs_df %>%
    bind_cols(
      metrics_df %>% select(-c(MID, groups, outcomes, stats))
    )

  s <- list(
    model = model,
    results = results_df
  )
  class(s) <- 'abaSummary'
  return(s)
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
    )
  r <- r %>%
    select(
      -c(.data$predictors, .data$covariates),
      -c(.data$std.error, .data$statistic, stats_obj, stats_fit)
    ) %>%
    mutate(
      coef = as.character(glue::glue(
        coef_fmt
      ))
    ) %>%
    select(-c(.data$estimate, .data$p.value))
  r <- r %>%
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
  r_split <- stats::setNames(
    split(r_nested, 1:nrow(r_nested)),
    r_nested$label
  ) %>%
    purrr::map(~.$data[[1]]) %>%
    purrr::map(~.[,colMeans(is.na(.)) < 1])

  r_split %>% purrr::iwalk(
    function(x,y) {
      cat('\n------------------------------------\n\n')
      cat(y, '\n\n')
      print(x)
    }
  )
}
