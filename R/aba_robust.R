
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
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit a standard model to predict a binary outcome
#' model <- data %>% aba_model() %>%
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
#'     verbose = TRUE
#'   )
#'
#' # plot results using the generic plot function
#' fig <- model_robust %>% aba_plot_metric()
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
  model_summary <- model %>% aba_summary()
  results_coefs_original <- model_summary$results$coefs %>%
    select(-c(conf_low, conf_high, pval))
  results_metrics_original <- model_summary$results$metrics %>%
    select(-c(conf_low, conf_high))

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
          list(
            coefs = model_noise_summary$results$coefs %>%
              select(-c(conf_low:pval)) %>%
              mutate(trial = idx),
            metrics = model_noise_summary$results$metrics %>%
              select(-c(conf_low:conf_high)) %>%
              mutate(trial = idx)
          )

        )
      }
    )
  results_coefs <- results %>% purrr::map('coefs') %>% bind_rows() %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      conf_low = quantile(estimate, 0.025),
      conf_high = quantile(estimate, 0.975),
      estimate = mean(estimate),
      .groups='keep'
    ) %>%
    ungroup() %>%
    select(group:term, estimate, conf_low, conf_high)

  results_metrics <- results %>% purrr::map('metrics') %>% bind_rows() %>%
    group_by(group, outcome, stat, predictor, term) %>%
    summarise(
      conf_low = quantile(estimate, 0.025),
      conf_high = quantile(estimate, 0.975),
      estimate = mean(estimate),
      .groups='keep'
    ) %>%
    ungroup() %>%
    select(group:term, estimate, conf_low, conf_high)

  object$results <- list(
    coefs = results_coefs,
    metrics = results_metrics
  )
  object$results_original <- model_summary$results

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

