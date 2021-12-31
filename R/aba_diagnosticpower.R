#' Caclulate diagnostic power based on a fitted aba model
#'
#' This function calculates power for a diagnostic test based on fitted results
#' of an aba model. The sensitivity/specificity and prevalance will be extracted
#' from the fitted model and 'n', 'power', or 'delta' can be calculated against
#' a null hypothesis performance. This function can be useful if you have pilot
#' data and want to plan a larger experiment based on this information rather
#' than making less data-driven assumptions.
#'
#' @param model fitted aba model. The model you want to run power analysis on.
#' @param delta value between 0 and 0.5. The half-size of the confidence interval
#'   that you want to (or will) acheive on the estimated sensitivity/specificity.
#'   Either delta or n should be specified.
#' @param n integer. The number of positive cases (for sensitivity) or negative
#'   cases (for specificity) that you want to (or need to) include. Either delta
#'   or n should be specified.
#' @param metric string: sens or spec. The metric of interest - either sensitivity
#'   or specificity
#'
#' @return a data frame with results from the `power.diagnostic.test` function
#'   for each fit in the aba model.
#' @export
#'
#' @examples
#' # We fit a diagnostic model on pilot data.
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl', DX_bl == 'MCI')
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(
#'     stat_glm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' # What will the delta on sensitivity be with 100 subjects
#' res_delta <- model %>% aba_diagnosticpower(n = 100)
#'
#' # How many subjects do we need to acheive a delta of 0.1 on sensitivity
#' res_n <- model %>% aba_diagnosticpower(delta = 0.1)
aba_diagnosticpower <- function(model,
                                delta = NULL,
                                n = NULL,
                                metric = c('sens', 'spec')) {
  metric <- match.arg(metric)
  if (is.null(delta) & is.null(n)) stop('Must supply one of delta or n')

  results <- model$results
  results <- results %>%
    mutate(
      result = purrr::map(
        fit,
        function(fit) {
          calculate_diagnostic_power(fit, delta, n, metric)
        }
      )
    ) %>%
    unnest_wider(.data$result)

  results <- results %>% select(-fit)

  results
}

calculate_diagnostic_power <- function(fit, delta, n, metric) {
  model_data <- get_model_data(fit)
  model_cutpoint <- get_model_cutpoint(model_data)
  model_metrics <- get_model_metrics(model_data, model_cutpoint)
  prev <- mean(model_data$.Truth==1, na.rm=T)

  if (metric == 'sens') {
    sens <- model_metrics$sens
    spec <- NULL
  } else if (metric == 'spec') {
    spec <- model_metrics$spec
    sens <- NULL
  }

  result <- MKmisc::power.diagnostic.test(
    sens = sens,
    spec = spec,
    n = n,
    delta = delta,
    prev = prev,
    power = 0.8
  )

  result_list <- list('delta' = result$delta,
                      'n' = as.integer(result$n),
                      'n_total' = as.integer(result$n1),
                      'prev' = result$prev,
                      'sens' = result$sens,
                      'power' = result$power)

  return(result_list)
}

get_model_metrics <- function(model_data, model_cutpoint) {
  model_data$.Predicted_binary <-
    as.factor(as.integer(model_data$.Predicted > model_cutpoint))
  tbl <- table(model_data$.Predicted_binary, model_data$.Truth)
  sens <- tbl['1','1'] / sum(tbl[,'1'])
  spec <- tbl['0','0'] / sum(tbl[,'0'])

  return(list('sens' = sens, 'spec' = spec))
}

get_model_cutpoint <- function(model_data) {

  cut_model <- OptimalCutpoints::optimal.cutpoints(
    .Predicted ~ .Truth,
    data = model_data,
    tag.healthy=0, direction='<', methods='Youden'
  )
  cut_val <- cut_model$Youden$Global$optimal.cutoff$cutoff[1]
  cut_val
}

get_model_data <- function(fit) {
  data <- stats::model.frame(fit)
  outcome <- colnames(data)[1]
  data$.Predicted <- stats::predict(fit, type='response')
  data$.Truth <- factor(data[[outcome]])
  data
}
