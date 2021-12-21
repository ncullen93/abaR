#' Create a glm stat object.
#'
#' This function creates a glm stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional logistic regression analysis using the `glm` function with
#' a binary outcome. Coefficients will be presented as odds ratios. Default
#' metrics include AUC.
#'
#' @param time string. The "time under risk" variable determining how long e.g.
#'   the individual has been in the study or when the individual got the disease.
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `glm` stat type.
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit model
#' model <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, ConvertedToDementia) %>%
#'   set_predictors(
#'     PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats(
#'     stat_cox(time = 'TimeUnderRiskDementia')
#'   ) %>%
#'   fit()
#'
#' ## summarise model
#' model_summary <- model %>% summary()
stat_cox <- function(time,
                     std.beta = FALSE,
                     complete.cases = TRUE) {
  #requireNamespace("survival", quietly = TRUE)

  struct <- list(
    'fns' = list(
      'formula' = formula_cox,
      'fit' = fit_cox,
      'tidy' = tidy_cox,
      'glance' = glance_cox
    ),
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    ),
    'extra_params' = list(
      'time' = time
    )
  )
  struct$stat_type <- 'cox'
  class(struct) <- 'abaStat'

  return(struct)
}

# helper function for stat_cox
formula_cox <- function(outcome, predictors, covariates, extra_params) {
  f <- glue('survival::Surv({extra_params$time}, {outcome}) ~ ')
  f <- glue('{f}{paste(covariates, collapse=" + ")}')
  if ((length(covariates) > 0) & (length(predictors) > 0)) f <- glue('{f} + ')
  f <- glue('{f}{paste(predictors, collapse=" + ")}')
  if (length(covariates) + length(predictors) == 0) f <- glue('{f}1')
  return(f)
}

# helper function for stat_cox
fit_cox <- function(formula, data, extra_params) {
  model <- survival::coxph(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  model
}

# helper function for stat_cox
tidy_cox <- function(model, predictors, covariates, ...) {
  if ('coxph.null' %in% class(model)) return(empty_tidy_data())
  x <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  x
}

# helper function for stat_cox
glance_cox <- function(fit, fit_basic, ...) {
  x <- broom::glance(fit)

  # pivot longer
  x <- x %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence intervals
  x <- x %>%
    mutate(
      conf.low = NA,
      conf.high = NA
    )

  x
}
