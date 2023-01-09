#' Create an lm stat object.
#'
#' This function creates a lm stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional linear regression analysis using the `lm` function.
#' Coefficients will be presented as beta coefficients. Default metrics include
#' adjusted R2.
#'
#' @param poly numeric or list. Whether to use polynomial regression.
#'   Supplying a single number will call poly(..., #NUMBER#, raw=TRUE) on
#'   every covariate and predictor. Supplying a list allows you to perform
#'   a polynomial expansion on specific variables. NULL means no polynomials.
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
#' ## polynomial regression ##
#' model <- data %>%
#'   aba_model() %>%
#'   set_outcomes(MRI_HIPP_bl) %>%
#'   set_covariates(
#'     c('AGE', 'EDUCATION')
#'   ) %>%
#'   set_stats(
#'     stat_lm(),
#'     stat_lm(poly = list(AGE=1)),
#'     stat_lm(poly = 2),
#'     stat_lm(poly = list(AGE = 2))
#'   ) %>%
#'   fit()
#'
#' model_summary <- model %>% summary()
stat_lm <- function(std.beta = FALSE,
                    complete.cases = TRUE,
                    poly = NULL) {
  fns <- list(
    'fns' = list(
      'formula' = formula_lm,
      'fit' = fit_lm,
      'tidy' = tidy_lm,
      'glance' = glance_lm,
      'evaluate' = evaluate_lm
    ),
    'extra_params' = list('poly' = poly),
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
formula_lm <- function(outcome, predictors, covariates, extra_params) {
  f <- paste0(outcome, " ~ ")

  covariates <- make_poly_formula(extra_params$poly, covariates)
  predictors <- make_poly_formula(extra_params$poly, predictors)

  if (length(covariates) > 0) {
    f <- paste0(f, paste(covariates, collapse = " + "))
    if (length(predictors) > 0) f <- paste0(f, ' + ')
  }
  if (length(predictors) > 0) f <- paste0(f, paste(predictors, collapse = " + "))
  if (length(covariates) + length(predictors) == 0) f <- paste0(f, '1')
  print(f)
  return(f)
}

# helper function for lm
fit_lm <- function(formula, data, ...) {
  model <- stats::lm(stats::formula(formula), data = data)
  model$call$formula <- stats::formula(formula)
  return(model)
}

# helpfer function for lm
tidy_lm <- function(model, predictors, covariates, ...) {
  broom::tidy(model, conf.int = TRUE)
}

# helper function for lm
glance_lm <- function(fit, fit_basic, ...) {

  # tidy glance
  glance_df <- broom::glance(fit)

  # r squared confidence interval
  ci_rsq <- psychometric::CI.Rsqlm(fit)

  # adjust the confidence interval because we use adj.r.squared
  rsq_diff <- glance_df$r.squared - glance_df$adj.r.squared
  ci_rsq$LCL <- ci_rsq$LCL - rsq_diff
  ci_rsq$UCL <- ci_rsq$UCL - rsq_diff

  # add comparison to null model
  if (!is.null(fit_basic)) {
    s <- stats::anova(fit, fit_basic)
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

evaluate_lm <- function(model, data_test) {

  # train data
  data_train <- broom::augment(model, newdata=tibble(stats::model.frame(model)))
  outcome <- names(data_train)[1]

  # test data with fitted results
  data_test <- broom::augment(model, newdata=data_test)
  data_test <- data_test %>% select(data_train %>% names())

  # calculate metrics
  train_metrics <- yardstick::metrics(
    data_train, truth = {{ outcome }}, estimate = .fitted
  ) %>% mutate(form = 'train')

  test_metrics <- yardstick::metrics(
    data_test, truth = {{ outcome }}, estimate = .fitted
  ) %>% mutate(form = 'test')

  x <- train_metrics %>%
    bind_rows(test_metrics) %>%
    select(-.estimator) %>%
    pivot_wider(names_from = .metric, values_from = .estimate)

  x
}

# helper function for lm
augment_lm <- function(model, newdata = NULL) {
  x <- broom::augment(model, newdata = newdata)
  x
}
