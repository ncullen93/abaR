#' Create an lm stat object.
#'
#' This function creates a lm stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional linear regression analysis using the `lm` function.
#' Coefficients will be presented as beta coefficients. Default metrics include
#' adjusted R2.
#'
#' @param std.beta logical. Whether to standardize model predictors and
#'   covariates prior to analysis.
#' @param complete.cases  logical. Whether to only include the subset of data
#'   with no missing data for any of the outcomes, predictors, or covariates.
#'   Note that complete cases are considering within each group - outcome
#'   combination but across all predictor sets.
#'
#' @return An abaStat object with `lm` stat type.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit lm model with continuous outcome variables
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CDRSB_bl, MMSE_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_covariates(AGE, GENDER, EDUCATION) %>%
#'   set_stats(
#'     stat_lm(std.beta = TRUE)
#'   ) %>%
#'   fit()
#'
#' # summarise model
#' model_summary <- model %>% summary()
#'
#' # plot results
#' fig1 <- model_summary %>% aba_plot_coef()
#' fig2 <- model_summary %>% aba_plot_metric()
#'
stat_lm <- function(std.beta = FALSE,
                   complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = formula_std,
    'fit_fn' = fit_lm,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'lm'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for lm
fit_lm <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# helper function for lm
aba_tidy.lm <- function(model, predictors, covariates, ...) {
  broom::tidy(model, conf.int = TRUE)
}

# helper function for lm
aba_glance.lm <- function(x, x0, ...) {

  # tidy glance
  glance_df <- broom::glance(x)

  # r squared confidence interval
  ci_rsq <- psychometric::CI.Rsqlm(x)

  # adjust the confidence interval because we use adj.r.squared
  rsq_diff <- glance_df$r.squared - glance_df$adj.r.squared
  ci_rsq$LCL <- ci_rsq$LCL - rsq_diff
  ci_rsq$UCL <- ci_rsq$UCL - rsq_diff

  # add comparison to null model
  if (!is.null(x0)) {
    s <- stats::anova(x, x0)
    null_pval <- s$`Pr(>F)`[2]
    glance_df <- glance_df %>%
      bind_cols(tibble::tibble(Pval = null_pval))
  }

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)


  ## add confidence intervals
  glance_df <- glance_df %>%
    left_join(
      tibble::tibble(
        term = c('adj.r.squared'),
        conf.low = c(ci_rsq$LCL),
        conf.high = c(ci_rsq$UCL)
      ),
      by = 'term'
    )

  return(glance_df)
}
