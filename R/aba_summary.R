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

aba_tidy <- function(model, predictors, covariates) {
  if ('lme' %in% class(model)) {
    time_var <- strsplit(
      as.character(model$call$random)[2],' | ',fixed=T
    )[[1]][1]
    broom.mixed::tidy(model, effects='fixed', conf.int=T) %>%
      select(-c(.data$df, .data$conf.low, .data$conf.high)) %>%
      filter(
        !(.data$term %in% predictors),
        .data$term != time_var
      ) %>%
      mutate(
        term = strsplit(.data$term, ':') %>%
          purrr::map_chr(~.[length(.)])
      )
  } else if ('gls' %in% class(model)) {
    time_var <- strsplit(as.character(model$call$weights)[2], ' | ')[[1]][3]
    x <- broom.mixed::tidy(model, conf.int=T) %>%
      select(-c('conf.low', 'conf.high')) %>%
      filter(
        !(.data$term %in% predictors)
      ) %>%
      filter(
        !startsWith(.data$term, time_var) | grepl('\\:', .data$term)
      )
  } else {
    if ('glm' %in% class(model)) {
      exp <- TRUE
    } else {
      exp <- FALSE
    }
    broom::tidy(model, exponentiate = exp)
  }
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
    filter(
      .data$term != '(Intercept)'
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
    )

  return(r)
}

metrics_summary <- function(model) {
  metric_vars <- c(
    'adj.r.squared',
    'R2',
    'AUC',
    'AIC',
    'nobs'
  )
  r <- model$results %>% rowwise() %>%
    mutate(
      stats_metrics = list(
        aba_glance(.data$stats_fit)
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
    purrr::map(~.$data[[1]] %>% select(-.data$MID))

  r_split %>% purrr::iwalk(
    function(x,y) {
      cat('\n------------------------------------\n\n')
      cat(y, '\n\n')
      print(x)
    }
  )
}
