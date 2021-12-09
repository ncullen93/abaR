#' Create an ancova stat object.
#'
#' This function creates an ancova stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional ancova analysis using the `lm` function.
#'
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `ancova` stat type.
#' @export
#'
#' @examples
#' x <- 1
stat_ancova <- function(std.beta = FALSE,
                        complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = formula_std,
    'fit_fn' = fit_ancova,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'ancova'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for ancova
fit_ancova <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# helper function for ancova
aba_tidy.ancova <- function(model, predictors, covariates) {
  tidy_df <- broom::glance(model)
  return(tidy_df)
}

# helper function for ancova
aba_glance.ancova <- function(x, x0, ...) {
  glance_df <- broom::glance(x)

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence interval
  glance_df <- glance_df %>%
    mutate(
      conf.low = NA,
      conf.high = NA
    )

  return(glance_df)
}



