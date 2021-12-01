

#' Create an lm stat to use for an aba model.
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
#' my_stat <- aba_lm()
#'
#' my_formula <- my_stat$formula_fn(
#'   outcome='ConvertedToAlzheimers',
#'   predictors=c('PLASMA_PTAU181_bl','PLASMA_NFL_bl'),
#'   covariates=c('AGE_bl','GENDER','EDUCAT')
#' )
#'
#' my_model <- my_stat$fit_fn(
#'   formula = my_formula,
#'   data = adni_sample
#' )
aba_lm <- function(std.beta = FALSE,
                   complete.cases = TRUE) {
  fns <- list(
    'formula_fn' = aba_formula_std,
    'fit_fn' = aba_fit_lm,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'lm'
  class(fns) <- 'abaStat'
  return(fns)
}

# fit a lm model
aba_fit_lm <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

#' @export
aba_tidy.lm <- function(model, predictors, covariates, ...) {
  broom::tidy(model, conf.int = TRUE)
}

#' @export
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
      bind_cols(
        tibble::tibble(Pval = null_pval)
      )
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





