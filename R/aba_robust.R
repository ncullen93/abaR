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
#' @param variation list the test-retest (or whatever) variation estimates
#'   for each predictor. This variability is assumed to standrd deviation of
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
                       variation,
                       ntrials = 10) {

  m <- list(
    'model' = model,
    'variation' = variation,
    'params' = list(
      'ntrials' = ntrials
    )
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
        ~.x * (1 + rnorm(nrow(data), 0, variation[[.x]] / 100))
      )
    )
  data_noise
}

fit.abaRobust <- function(object) {
  ntrials <- object$params$ntrials
  model <- object$model
  predictors <- model %>% get_predictors()
  data_original <- model$data

  # for each trial, simulate noisy data, then re-fit/re-summarise model
  noise_summary_results <- 1:ntrials %>%
    purrr::map(
      function() {
        data_noise <- simulate_data_noise(
          data_original, object$variation
        )

        model_noise <- model %>% set_data(data_noise) %>% fit()
        model_noise_summary <- model_noise %>% aba_summary()
        model_noise_summary$results
      }
    )
  object$results <- noise_summary_results
  return(object)
}




