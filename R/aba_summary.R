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
      metrics_df %>% select(-c(groups, outcomes, stat)),
      by = 'MID'
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
    '{sprintf("%.1f", estimate)}',
    '(P={sprintf("%.4f", p.value)})'
  )

  # coefficients
  r <- model$results %>%
    dplyr::mutate(
      dplyr::across(
        names(model$spec$stats),
        ~purrr::map(
          .x,
          broom::tidy
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
      # individual metrics
      .glance = purrr::map(
        fit,
        broom::glance
      ),
      # metrics requiring the entire model group (e.g., p-value versus basic)
      .glance_extra = purrr::map2(
        fit, outcomes,
        glance_extra
      )
    ) %>%
    tidyr::unnest(
      c(
        .data$.glance,
        .data$.glance_extra
      )
    ) %>%
    dplyr::select(
      MID,
      groups, outcomes, stat,
      any_of(metric_vars)
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
