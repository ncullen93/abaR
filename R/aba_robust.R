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
# variation <- list('PLASMA_ABETA_bl'=3.4, 'PLASMA_PTAU181_bl'=9.3)
# df_noise <- simulate_data_noise(df, variation)
simulate_data_noise <- function(data, variation) {
  predictors <- names(variation)

  data_noise <- data %>%
    mutate(
      across(
        all_of(predictors),
        ~.x * (1 + rnorm(nrow(data),
                         0,
                         variation[[cur_column()]]) / 100)
      )
    )
  data_noise
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
  object$results_original <- aba_summary(model)$results

  # for each trial, simulate noisy data, then re-fit/re-summarise model
  if (object$verbose) pb <- progress::progress_bar$new(total = ntrials)

  noise_summary_results <- 1:ntrials %>%
    purrr::map(
      function(idx) {
        if (object$verbose) pb$tick()

        data_noise <- simulate_data_noise(
          data_original, object$variation
        )

        model_noise <- model %>% set_data(data_noise) %>% fit()
        model_noise_summary <- model_noise %>% aba_summary()
        return(model_noise_summary$results)
      }
    )
  object$results <- noise_summary_results

  ## summarise
  #compare_res <- function(res, res_orig) {
  #  res[,c('AUC', 'AIC')] <- 100*(
  #    (res[,c('AUC', 'AIC')] - res_orig[,c('AUC', 'AIC')]) /
  #      res_orig[,c('AUC', 'AIC')])
  #  res
  #}
#
  #res_orig <- object$results_original
  #res <- object$results
  #res_diff <- res %>% purrr::map(~compare_res(., res_orig)) %>% bind_rows()
  #res_sum <- res_diff %>% group_by(MID, groups, outcomes, stats) %>%
  #  summarise(
  #    dAUC_mean = mean(AUC),
  #    dAUC_lo = quantile(AUC, 0.025),
  #    dAUC_hi = quantile(AUC, 0.975),
  #    dAIC_mean = mean(AIC),
  #    dAIC_lo = quantile(AIC, 0.025),
  #    dAIC_hi = quantile(AIC, 0.975),
  #    Cut_mean = mean(Cut),
  #    Cut_lo = quantile(Cut, 0.025),
  #    Cut_hi = quantile(Cut, 0.975)
  #  )
#
  object$results <- object$results %>% bind_rows()
  #object$results_difference <- res_diff
  #object$results_summary <- res_sum

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
aba_plot_metric.abaRobust <- function(object) {
  ggplot(object$results_difference, aes(x=MID, y=AUC)) +
    geom_jitter(width=0.1, alpha=0.1, size=1) +
    stat_summary(fun = mean, geom = "crossbar",
                 size=0.5, width=0.75) +
    stat_summary(fun.min = function(z) {mean(z)-sd(z)},
                 fun.max = function(z) {mean(z)+sd(z)},
                 size = 1, width=0.5,
                 geom = "errorbar") +
    geom_hline(yintercept=0, linetype='dashed') +
    facet_wrap(groups ~ outcomes) +
    ylab('\u0394AUC (%)') +
    theme_classic(base_size = 18) +
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


