#' Create a roc stat object.
#'
#' This function creates a roc stat object which can be passed as input
#' to the `set_stats()` function when building an aba model. This stat performs
#' a traditional ROC / cutpoint analysis from a binary outcome using the
#' `optimal.cutpoints` function from the `OptimalCutpoints` package. Note that
#' outcomes for this model should be binary and coded as 0 = healthy and
#' 1 = disease.
#' Coefficients will be presented as the optimal cutpoint for the model derived
#' from Youden's index (or whatever method is specified).
#' Default metrics include AUC.
#'
#' @param direction '<' or '>. Which direction to interpret as being further
#'   from the healthy value. '<' is the default value and is interpreted as
#'   increasing predictor values are worse. '>' is therefore interpreted as
#'   higher predictor values are closer to healthy (outcome value of 0).
#' @param method string. Which method to use to calculate the optimal cutoff
#'   value. See the `OptimalCutpoints::optimal.cutpoints` function for more
#'   info.
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
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # fit a roc model to predict a binary outcome
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl %in% c('MCI', 'AD')
#'   ) %>%
#'   set_outcomes(CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(PLASMA_PTAU181_bl, PLASMA_NFL_bl) %>%
#'   set_stats(
#'     stat_roc(method='Youden')
#'   ) %>%
#'   fit()
#'
#' # summarise model
#' model_summary <- model %>% summary()
#'
#' # if using predictors where higher values are better, then flip direction
#' model2 <- model %>%
#'   set_predictors(PLASMA_ABETA_bl) %>%
#'   set_stats(
#'     stat_roc(direction = '>')
#'   ) %>%
#'   fit()
#' model2_summary <- model2 %>% aba_summary()
#'
stat_roc <- function(direction = '<',
                     method = 'Youden',
                     std.beta = FALSE,
                     complete.cases = TRUE) {

  fns <- list(
    'formula_fn' = formula_roc,
    'fit_fn' = fit_roc,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    ),
    'extra_params' = list(
      'direction' = direction,
      'method' = method
    )

  )
  fns$stat_type <- 'roc'
  class(fns) <- 'abaStat'
  return(fns)
}

# helper function for stat_roc
formula_roc <- function(outcome, predictors, covariates, ...) {
  if (length(predictors) > 1) stop('ROC predictors should only be length == 1.')
  predictor <- predictors[1]
  if (is.null(predictor)) predictor <- 1
  f <- as.character(glue('{predictor} ~ {outcome}'))
  return(f)
}

# helper function for stat_roc
fit_roc <- function(formula, data, extra_params) {

  model <- OptimalCutpoints::optimal.cutpoints(
    stats::formula(formula),
    data = data.frame(data),
    tag.healthy=0,
    direction=extra_params$direction,
    methods=extra_params$method
  )
  model$call$X <- stats::formula(formula)
  class(model) <- c('roc', class(model))
  return(model)
}

# helper function for stat_roc
aba_tidy.roc <- function(model, predictors, covariates, ...) {
  # coefficient is the cutoff value
  cut_val <- model[[model$methods[1]]]$Global$optimal.cutoff$cutoff[1]
  predictor <- as.character(model$call$X)[2]
  cut_vals <- ifelse(predictors==predictor, cut_val, NA)
  x <- tibble::tibble(
    term = predictors,
    estimate = cut_vals,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )


 return(x)
}

# helper function for stat_roc
aba_glance.roc <- function(x, x0, ...) {
  auc <- x[[x$methods[1]]]$Global$measures.acc$AUC
  nobs <- nrow(x$data)

  # create initial glance df
  glance_df <- tibble::tibble(
    AUC = unname(auc[1]),
    nobs = nobs,
    AIC = NA
  )

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence intervals
  glance_df <- glance_df %>%
    left_join(
      tibble::tibble(
        term = c('AUC'),
        conf.low = c(unname(auc)[2]),
        conf.high = c(unname(auc)[3])
      ),
      by = 'term'
    )
  return(glance_df)
}
