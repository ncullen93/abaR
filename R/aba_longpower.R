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
                          dropout = 0) {

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
    sig_level = as.vector(sig_level),
    dropout = as.vector(dropout)
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



  if (!is.null(names(object$spec$groups))) {
    res_df <- res_df %>%
      mutate(
        group = factor(group, levels=object$spec$groups, labels=names(object$spec$groups))
      )
  }

  if (!is.null(names(object$spec$outcomes))) {
    res_df <- res_df %>%
      mutate(
        outcome = factor(outcome, levels=object$spec$outcomes,
                         labels=names(object$spec$outcomes))
      )
  }

  struct <- list(
    object = object,
    params = list(
      n = n,
      pct_change = pct_change,
      power = power,
      t_length = t_length,
      t_freq = t_freq,
      sig_level = sig_level,
      dropout = dropout
    ),
    results = res_df
  )

  class(struct) <- 'abaLongpower'
  struct
}


calculate_power <- function(fit, param_df) {
  solve_var <- 'n'

  param_df <- param_df %>%
    rowwise() %>%
    mutate(
      {{ solve_var }} := calculate_power_individual(
        fit,
        .data$t_length,
        .data$t_freq,
        .data$n,
        .data$pct_change,
        .data$power,
        .data$sig_level,
        .data$dropout
      )
    ) %>%
    ungroup()

  param_df
}


calculate_power_individual <- function(
  fit, t_length, t_freq, n, pct_change, power, sig_level, dropout
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

  total_n <- res$N * (1 + dropout)

  return(
    total_n
  )
}

#' @export
aba_plot.abaLongpower <- function(object, ...) {
  res_df <- object$results %>%
    mutate(pct_change = paste0(sprintf('%.0f', 100*pct_change),'%'),
           power = factor(paste('Power = ', power)))

  g <- res_df %>%
    ggplot(aes(x = pct_change, y = n*1.2, color = power, group = power)) +
    geom_line(size=2) +
    facet_wrap(paste0('Outcome: ', outcome) ~ paste0('Inclusion: ', group)) +
    xlab('Treatment effect') +
    ylab('Sample size') +
    ylim(c(0, max(object$results$n*1.2)*1.05)) +
    theme_classic(base_size=18) +
    theme(legend.position='top',
          legend.margin = margin(5, 1, 1, 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          legend.key.width = unit(0.75,"cm"),
          legend.text=element_text(size=14),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 16, vjust = 1.25),
          plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
            colour = "black",
            size = 0.2, linetype = "dotted"))
  g <- ggpubr::set_palette(g, 'jama')
  g
}

#' @export
print.abaLongpower <- function(object, ...) {
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



