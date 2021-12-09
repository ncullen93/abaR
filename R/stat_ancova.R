#' Create an ancova stat to use for an aba model.
#'
#' @return
#' list of the following functions:
#'   * `formula_fn`: create a formula
#'   * `fit_fn`: fit a model
#'   * `evaluate_fn`: evaluate a model
#'
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

# fit a lm model
fit_ancova <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

#' @export
aba_tidy.ancova <- function(model, predictors, covariates) {
  tidy_df <- broom::glance(model)
  return(tidy_df)
}

#' @export
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



