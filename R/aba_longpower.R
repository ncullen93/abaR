#' Run power analysis on a longitudinal-based aba model.
#'
#' This function allows users to calculate power, required sample size, or
#' percent change (i.e., treatment effect) on a fitted aba model that uses
#' longitudinal stats such as `stat_lme`. Whichever argument is left as NULL
#' will be the argument for which this function solves. Users can specify a
#' range of values for any function arguments to test different assumptions.
#'
#' Power is calculated using the `lmmpower` function from the `longpower`
#' package in R. Please see documentation there to get a better understanding
#' of the actual formulas involved.
#'
#' @param object aba model. The fitted aba model on which to calculate power.
#'   This model should feature a longitudinal aba stat (e.g., `stat_lme`)
#' @param n integer. The total minimum required sample size.
#' @param pct_change double between 0 - 1. The expected treatment effect which
#'   means the percent by which the slope will decrease in the treatment group.
#'   Note that "absolute" treatment effects are not currently supported.
#' @param power double between 0 - 1. The expected power to detect the
#'   treatment effect.
#' @param t_length double. The expected duration of the clinical trial. Note
#'   that this value should be on the same scale as the `time` argument used
#'   to fit the longitudinal aba stat. A longer trial will generally result
#'   in smaller sample size or higher power.
#' @param t_freq double. The expected frequency of endpoint sampling during
#'   the trial. This value should be less than `t_length`. Note that this
#'   value should also be on the same scale as the `time` argument used to fit
#'   the longitudinal aba stat.
#' @param sig_level double between 0 - 1. The required alpha level. There is
#'   usually little reason to change this from the default of 0.05.
#' @param dropout double between 0 - 1. The expected overall drop-out rate
#'   during the trial. Note that we handle dropout in a fairly crude way by
#'   simply multiplying the sample size by (1 + dropout) instead of, say,
#'   sampling dropout from some distribution at each visit.
#'
#' @return An aba longpower object which can be plotted and printed in special
#'   ways and which contains all the resulting calculations.
#'
#' @export
#'
#' @examples
#'
#' # use only two year follow-up data; filter by some basic AD trial criteria
#' data <- adnimerge %>%
#'   dplyr::filter(
#'     VISCODE %in% c('bl', 'm06', 'm12', 'm24'),
#'     DX_bl %in% c('MCI','AD'),
#'     CDR_bl %in% c(0.5, 1),
#'     MMSE_bl >= 20, MMSE_bl <= 28
#'   )
#'
#' # fit an aba model with an lme stat to get a longitudinal model
#' model <- data %>% aba_model() %>%
#'   set_outcomes(CDRSB, ADAS13) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(stat_lme(id = 'RID', time = 'YEARS_bl')) %>%
#'   fit()
#'
#' # summarize aba model - not necessary here but good to see results
#' model_summary <- model %>% summary()
#'
#' # run power analysis on the fitted aba model with various assumptions
#' # e.g., treatment effect between 25 - 35%; power between 80 - 90%
#' pwr <- model %>%
#'   aba_longpower(
#'     n = NULL,
#'     pct_change = c(0.25, 0.30, 0.35),
#'     power = c(0.8, 0.85, 0.9),
#'     t_length = 2,
#'     t_freq = 0.25,
#'     dropout = 0.2
#'   )
#'
#' # generate a standard results figure from the power results
#' fig <- pwr %>% aba_plot()
#'
#' # add better inclusion criteria (CSF & CSF+MRI) to the aba model and refit.
#' model2 <- model %>%
#'   set_groups(
#'     everyone(),
#'     (CSF_ABETA_bl < 880) & (CSF_PTAU_bl > 24),
#'     (CSF_ABETA_bl < 880) & (CSF_PTAU_bl > 24) & (MRI_HIPP_bl < 6000),
#'     labels = c('DX + COG', 'DX + COG + CSF', 'DX + COG + CSF + MRI')
#'   ) %>%
#'   fit()
#'
#' # summarise model fit - again, not necessary but good to see slopes
#' model2_summary <- model2 %>% summary()
#'
#' pwr2 <- model2 %>%
#'   aba_longpower(
#'     n = NULL,
#'     pct_change = c(0.25, 0.3, 0.35),
#'     power = c(0.8, 0.85, 0.9),
#'     t_length = 2,
#'     t_freq = 0.25,
#'     dropout = 0.2
#'   )
#'
#' fig2 <- pwr2 %>% aba_plot()
#'
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

  # run power analysis
  res_df <- res_df %>%
    rowwise() %>%
    mutate(
      power_fit = list(calculate_power(
        .data$fit, param_df
      ))
    )

  # unnest
  res_df <- res_df %>%
    select(
      group:predictor, power_fit
    ) %>%
    unnest(power_fit)

  struct <- list(
    results = res_df,
    params = list(
      n = n,
      pct_change = pct_change,
      power = power,
      t_length = t_length,
      t_freq = t_freq,
      sig_level = sig_level,
      dropout = dropout
    ),
    model = object
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
print.abaLongpower <- function(x, ...) {
  object <- x
 results <- object$results
 results <- results %>%
   group_by(group, outcome, stat, predictor) %>%
   nest() %>%
   mutate(
     label = glue('{group} | {outcome} | {stat} | {predictor}')
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



