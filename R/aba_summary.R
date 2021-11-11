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
    dplyr::left_join(
      metrics_df %>% dplyr::select(-c(.data$groups,
                                      .data$outcomes,
                                      .data$stat)),
      by = 'MID'
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
    time_var <- strsplit(as.character(model$call$random)[2],' | ',fixed=T)[[1]][1]
    broom.mixed::tidy(model, effects='fixed', conf.int=T) %>%
      select(-c(df, conf.low, conf.high)) %>%
      filter(
        !(term %in% predictors),
        term != time_var
      ) %>%
      mutate(
        term = strsplit(term, ':') %>%
          map_chr(~.[length(.)])
      )
  } else if ('gls' %in% class(model)) {
    time_var <- strsplit(as.character(model$call$weights)[2], ' | ')[[1]][3]
    x <- broom.mixed::tidy(model, conf.int=T) %>%
      select(-c(conf.low, conf.high)) %>%
      filter(
        !(term %in% predictors)
      ) %>%
      filter(
        !startsWith(term, time_var) | grepl('\\:', term)
      )
  } else {
    broom::tidy(model)
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
  r <- model$results %>%
    dplyr::mutate(
      dplyr::across(
        names(model$spec$stats),
        ~purrr::map(
          .x,
          ~aba_tidy(., all_predictors, all_covariates)
        )
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(model$spec$stats)),
      names_to = 'stat',
      values_to = 'fit'
    ) %>%
    tidyr::unnest(
      .data$fit
    ) %>%
    dplyr::filter(
      .data$term != '(Intercept)'
    ) %>%
    dplyr::select(
      -c(.data$predictors:.data$stats),
      -c(.data$std.error, .data$statistic)
    ) %>%
    dplyr::mutate(
      coef = as.character(glue::glue(
        coef_fmt
      ))
    ) %>%
    dplyr::select(-c(.data$estimate, .data$p.value)) %>%
    tidyr::pivot_wider(
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
  r <- model$results %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(model$spec$stats)),
      names_to = 'stat',
      values_to = 'fit'
    ) %>%
    dplyr::group_by(
      .data$groups,
      .data$outcomes,
      .data$stat
    ) %>%
    dplyr::mutate(
      # individual model metrics (tidy/custom)
      .glance = purrr::map(
        fit,
        aba_glance
      )
    ) %>%
    tidyr::unnest(
      c(.data$.glance)
    ) %>%
    dplyr::select(
      .data$MID,
      .data$groups,
      .data$outcomes,
      .data$stat,
      dplyr::any_of(metric_vars)
    ) %>%
    dplyr::ungroup()

  return(r)
}

#' @export
print.abaSummary <- function(x, ...) {
  r_nested <- x$results %>%
    dplyr::group_by(
      .data$groups,
      .data$outcomes,
      .data$stat
    ) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      label = glue::glue('{groups} | {outcomes} | {stat}')
    )
  r_split <- stats::setNames(
    split(r_nested, 1:nrow(r_nested)),
    r_nested$label
  ) %>%
    purrr::map(~.$data[[1]] %>% dplyr::select(-.data$MID))

  r_split %>% purrr::iwalk(
    function(x,y) {
      cat('\n------------------------------------\n\n')
      cat(y, '\n\n')
      print(x)
    }
  )
}
