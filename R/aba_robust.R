#' Create an aba robust object
#'
#' An aba robust object is composed of the following:
#'   - data: a data.frame to be used to fit the statistical models
#'   - spec: the specification for the aba model composed of the following:
#'     - groups: which parts of the data to fit separate models on
#'     - outcomes: which variables to use as dependent variables
#'     - covariates: which variables to use as fixed independent variables in
#'         every single model that is fit
#'     - predictors: which variables to use as independent variables, but never
#'         together in the same model
#'   - fits: the fitted statistical models once `fit()` is called
#'
#' @param model abaModel the aba model to use for the object
#' @param bias list the test-retest (or whatever) bias estimates
#'   for each predictor. This bias is assumed to be runif from 0 - bias value.
#' @param variation list the test-retest (or whatever) variation estimates
#'   for each predictor. This variability is assumed to be standrd deviation of
#'   of a normal distribution to be simulated.
#' @param ntrials integer number of noise simulations to run
#'
#' @return An abaRobust object
#'
#' @export
#'
#' @examples
#' x <- 1
aba_robust <- function(model,
                       bias = NULL,
                       variation = NULL,
                       ntrials = 100,
                       verbose = TRUE) {

  if (is.null(bias) & is.null(variation)) stop('Must give bias or variation.')

  m <- list(
    'model' = model,
    'bias' = bias,
    'variation' = variation,
    'params' = list(
      'ntrials' = ntrials
    ),
    'verbose' = verbose
  )
  class(m) <- 'abaRobust'
  return(m)
}

# simulate noise on data
# example:
# df <- adni_sample
# bias <- list('PLASMA_ABETA_bl' = 4.1, 'PLASMA_PTAU181_bl' = 8.3)
# variation <- list('PLASMA_ABETA_bl' = 3.4, 'PLASMA_PTAU181_bl' = 9.3)
# df_noise <- simulate_data_noise(df, variation)
simulate_data_noise <- function(data, bias, variation) {
  # add bias
  if (!is.null(bias)) {
    b_predictors <- names(bias)
    data <- data %>%
      mutate(
        across(
          all_of(b_predictors),
          ~.x * (1 + runif(n = nrow(data),
                           min = 0,
                           max = 2*bias[[cur_column()]]) / 100)
        )
      )

  }

  # add variance
  if (!is.null(variation)) {
    v_predictors <- names(variation)
    data <- data %>%
      mutate(
        across(
          all_of(v_predictors),
          ~.x * (1 + rnorm(n = nrow(data),
                           mean = 0,
                           sd = variation[[cur_column()]]) / 100)
        )
      )
  }

  data
}

#' Fit an aba robust object
#'
#' This will trigger the fitting of an aba robust object to determine the
#' effect of simulated noise/variation/test-retest on model coefficients
#' and preformance.
#'
#' @param object abaModel. The aba model whose robustness will be tested. This
#'   aba model should already be fit itself prior to this.
#' @param ... additional parameters.
#'
#' @return abaRobust
#' @export
#' @examples
#' x <- 1
fit.abaRobust <- function(object, ...) {

  ntrials <- object$params$ntrials
  model <- object$model
  data_original <- model$data

  # get original model summary results
  results_original <- aba_summary(model)$results %>%
    select(-c(lo, hi, pval))

  # for each trial, simulate noisy data, then re-fit/re-summarise model
  if (object$verbose) pb <- progress::progress_bar$new(total = ntrials)

  results <- 1:ntrials %>%
    purrr::map(
      function(idx) {
        if (object$verbose) pb$tick()

        data_noise <- simulate_data_noise(
          data_original, object$bias, object$variation
        )

        model_noise <- model %>% set_data(data_noise) %>% fit()
        model_noise_summary <- model_noise %>% aba_summary()
        return(
          model_noise_summary$results %>%
            mutate(trial = idx)
        )
      }
    ) %>%
    bind_rows()

  results <- results %>%
    left_join(
      results_original %>% rename(est_orig = est),
      by = c('MID','groups','outcomes','stats','term','form')
    ) %>%
    mutate(
      est_diff = 100 * (est - est_orig) / (est_orig)
    )


  results_summary <- results %>%
    group_by(MID, groups, outcomes, stats, term, form) %>%
    summarise(
      est = mean(est),
      lo = quantile(est, 0.025),
      hi = quantile(est, 0.975),
      est_diff = mean(est_diff, na.rm=T),
      lo_diff = quantile(est_diff, 0.025, na.rm=T),
      hi_diff = quantile(est_diff, 0.975, na.rm=T),
      .groups='keep'
    )

  object$results_original <- results_original
  object$results <- results
  object$results_summary <- results_summary

  return(object)
}


#' Plot AUC difference of aba robust object
#'
#' @param object abaRobust. object to plot results from
#'
#' @return ggplot
#' @export
#'
#' @examples
#' x <- 1
aba_plot_metric.abaRobust <- function(object,
                                      metric = NULL,
                                      model_labels = NULL,
                                      group_labels = NULL,
                                      outcome_labels = NULL,
                                      ...) {
  params <- list(...)
  metric <- 'AUC'
  if ('metric' %in% names(params)) metric <- params$metric

  plot_df <- object$results

  if (!is.null(model_labels)) {
    plot_df <- plot_df %>%
      mutate(MID = factor(MID, labels=model_labels))
  }
  if (!is.null(group_labels)) {
    plot_df <- plot_df %>%
      mutate(groups = factor(groups,
                             levels=names(group_labels),
                             labels=unname(unlist(group_labels))))
  }
  if (!is.null(outcome_labels)) {
    plot_df <- plot_df %>%
      mutate(outcomes = factor(outcomes,
                             levels=names(outcome_labels),
                             labels=unname(unlist(outcome_labels))))
  }

  plot_df %>%
    filter(term == metric) %>%
    ggplot(aes(x=MID, y=est_diff, color=MID)) +
    geom_jitter(width=0.1, alpha=0.1, size=1) +
    stat_summary(fun = mean, geom = "crossbar",
                 size=0.5, width=0.75) +
    stat_summary(fun.min = function(z) {mean(z)-sd(z)},
                 fun.max = function(z) {mean(z)+sd(z)},
                 size = 1, width=0.5,
                 geom = "errorbar") +
    geom_hline(yintercept=0, linetype='dashed') +
    facet_wrap(.~paste0(.data$outcomes,' | ', .data$groups)) +
    ylab(glue('Δ{metric} (%)')) +
    theme_classic(base_size = 16) +
    theme(legend.position='none',
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          #plot.margin = unit(c(1, 1,0.5,0.5), "lines"),
          panel.spacing = unit(1.5, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 18, vjust = 1.25)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
            colour = "black",
            size = 0.2, linetype = "dotted"))
}


