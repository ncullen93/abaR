
#' Evaluate the robustness of an aba model to systematic and random error.
#'
#' This function allows you to test how adding bias to predictor values or how
#' adding random error to predictor values affects the model coefficients and
#' performance metrics (e.g., AUC, R2, etc) as a result. This function is useful
#' when you have test-retest estimates of biomarkers and want to test what
#' effect this has on diagnostic or prognostic modelling.
#'
#' @param model an aba model. The fitted aba model to perform robustness
#'   analysis on.
#' @param bias double or list of doubles. If one value is given, this is the
#'   percent value added or subtracted to all predictor values at each trial. If
#'   this is a list, the names of the list should be the predictors to apply
#'   bias to and the values should be the bias to apply to each predictor.
#' @param variation double or list of doubles. This is the percent value which
#'   represents the standard deviation of a normal distribution. The random
#'   error values will be randomly sampled from this normal distribution for
#'   each data row (participant) at each trial.
#' @param ntrials integer. Number of trials to run. A trial represents a
#'   different random sampling of the variation distribution. This does not
#'   have any effect for bias because the bias value is always the same.
#' @param verbose logical. Whether to include a progress bar to track trials.
#'
#' @return an abaRobust object which contains results from the robustness
#'   analysis that displays how model coefficients and metrics changed when
#'   bias and variation was injected into the predictors.
#' @export
#'
#' @examples
#'
#' # read and process data
#' df <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit a standard model to predict a binary outcome
#' model <- df %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(PLASMA_PTAU181_bl, PLASMA_NFL_bl) %>%
#'   set_stats(stat_roc(method='Youden', direction = '<')) %>%
#'   aba_fit()
#'
#' # summarise model (these are the original results)
#' model_summary <- model %>% aba_summary()
#'
#' # specify test-retest variation for predictors (defined as percent change)
#' # this can be theoretical values (e.g. 5, 10, 15, 20) or derived from
#' # test-retest studies where you measured the biomarkers twice
#' variation <- list(
#'   'PLASMA_PTAU181_bl' = 9.5,
#'   'PLASMA_NFL_bl' = 20.2
#' )
#'
#' # test robustness of the fitted aba model to this robustness
#' model_robust <- model %>%
#'   aba_robust(
#'     variation = variation,
#'     ntrials = 10,
#'     verbose = T
#'   )
#'
#' # plot results using the generic plot function
#' fig <- model_robust %>% aba_plot()
#'
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

  m <- fit_robust(m)

  return(m)
}


# helper function to run the robustness analysis
fit_robust <- function(object, ...) {

  ntrials <- object$params$ntrials
  model <- object$model
  data_original <- model$data

  # get original model summary results
  results_original <- aba_summary(model)$results %>%
    select(-c(conf_low, conf_high, pval))

  # for each trial, simulate noisy data, then re-fit/re-summarise model
  if (object$verbose) pb <- progress::progress_bar$new(total = ntrials)

  results <- 1:ntrials %>%
    purrr::map(
      function(idx) {
        if (object$verbose) pb$tick()

        data_noise <- simulate_data_noise(
          data_original, object$bias, object$variation
        )

        model_noise <- model %>% set_data(data_noise) %>% aba_fit()
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
      results_original %>% rename(estimate_original = estimate),
      by = c('predictor_set','group','outcome','stat','term','form')
    ) %>%
    mutate(
      estimate_diff = estimate - estimate_original
    )


  results_summary <- results %>%
    group_by(predictor_set, group, outcome, stat, term, form) %>%
    summarise(
      conf_low = quantile(estimate, 0.025),
      conf_high = quantile(estimate, 0.975),
      estimate = mean(estimate),
      conf_low_diff = quantile(estimate_diff, 0.025, na.rm=T),
      conf_high_diff = quantile(estimate_diff, 0.975, na.rm=T),
      estimate_diff = mean(estimate_diff, na.rm=T),
      .groups='keep'
    ) %>%
    select(predictor_set:form,
           estimate, conf_low, conf_high,
           estimate_diff, conf_low_diff, conf_high_diff) %>%
    ungroup() %>%
    arrange(predictor_set, group, outcome, stat, form, term)

  object$results_original <- results_original %>%
    arrange(predictor_set, group, outcome, stat, form, term)
  object$results <- results
  object$results_summary <- results_summary

  return(object)
}

# helper function to add bias and variation to data
simulate_data_noise <- function(data, bias, variation) {
  # add bias
  if (!is.null(bias)) {
    b_predictors <- names(bias)
    data <- data %>%
      mutate(
        across(
          all_of(b_predictors),
          ~.x * (1 + bias)
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

#' Plot results of an aba robust object
#'
#' @param object an aba robust object. The object whose results will be plotted.
#'
#' @return a ggplot
#' @export
aba_plot.abaRobust <- function(object, ...) {

  metric <- 'AUC'
  plot_df <- object$results

  g <- plot_df %>%
    filter(term == metric) %>%
    ggplot(aes(x=predictor_set, y=estimate_diff, color=predictor_set)) +
    geom_jitter(width=0.1, alpha=0.1, size=1) +
    stat_summary(fun = mean, geom = "crossbar",
                 size=0.5, width=0.75) +
    stat_summary(fun.min = function(z) {mean(z)-sd(z)},
                 fun.max = function(z) {mean(z)+sd(z)},
                 size = 1, width=0.5,
                 geom = "errorbar") +
    geom_hline(yintercept=0, linetype='dashed') +
    facet_wrap(.~paste0(.data$outcome,' | ', .data$group)) +
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

  g
}


