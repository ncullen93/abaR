#' Run power analysis on longitudinal data
#'
#' @param object object
#' @param n n
#' @param t t
#' @param pct_change pct_change
#' @param power power
#' @param sig_level sig_level
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_longpower <- function(object,
                          n = NULL,
                          pct_change = NULL,
                          power = NULL,
                          t_length = NULL,
                          t_freq = NULL,
                          sig_level = 0.05,
                          nboot = 0) {

  if (is.null(n)) n <- NA
  if (is.null(pct_change)) pct_change <- NA
  if (is.null(power)) power <- NA
  if (is.null(t_length) | is.null(t_freq)) stop('Must give t_length t_freq.')

  res_df <- object$results

  # make parameter dataframe
  param_df <- list(
    t_length = t_length,
    t_freq = t_freq,
    n = n,
    pct_change = as.vector(pct_change),
    power = power,
    sig_level = as.vector(sig_level)
  ) %>%
    purrr::cross_df()

  param_df <- param_df %>%
    mutate(PID = paste0('P', 1:nrow(param_df))) %>%
    select(PID, everything())

  # run power analysis
  res_df <- res_df %>%
    rowwise() %>%
    mutate(
      power_fit = list(calculate_power(
        .data$stat_fit, param_df
      ))
    )

  # unnest
  res_df <- res_df %>%
    select(
      group:predictor_set, power_fit
    ) %>%
    unnest(power_fit)

  struct <- list(
    object = object,
    params = list(
      n = n,
      pct_change = pct_change,
      power = power,
      t_length = t_length,
      t_freq = t_freq,
      sig_level = sig_level,
      nboot = nboot
    ),
    results = res_df
  )

  class(struct) <- 'abaPower'
  struct
}


calculate_power <- function(fit, param_df) {
  solve_var <- 'n'

  param_df <- param_df %>%
    rowwise() %>%
    mutate(
      {{ solve_var }} := calculate_power_individual(
        fit,
        .data$t_length, .data$t_freq,
        .data$n, .data$pct_change,
        .data$power, .data$sig_level
      )
    ) %>%
    ungroup()
  param_df
}


calculate_power_individual <- function(
  fit, t_length, t_freq, n, pct_change, power, sig_level
) {

  t <- seq(0, t_length, t_freq)

  if (is.na(n)) n <- NULL
  if (is.na(power)) power <- NULL
  if (is.na(pct_change)) pct_change <- NULL

  res <- longpower::lmmpower(
    fit, n=n, t=t,
    pct.change=pct_change,
    sig.level=sig_level, power=power,
    method='edland'
  )
  return(
    res$N
  )
}

#' @export
print.abaPower <- function(object, ...) {
 results <- object$results
 results <- results %>%
   group_by(group, outcome, stat, predictor_set) %>%
   nest() %>%
   mutate(
     label = glue('{group} | {outcome} | {stat} | {predictor_set}')
   )

 results_split <- stats::setNames(
   split(results, 1:nrow(results)),
   results$label
 )

 results_split %>% purrr::iwalk(
   function(info, label) {
     nchar_label <- nchar(label)
     cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')
     cat(label)
     cat('\n'); cat(rep('-', nchar_label), sep=''); cat('\n')
     print(info$data[[1]])
   }
 )
}



