#' Create a roc stat to use for an aba model.
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
#' my_stat <- aba_roc()
#'
#' my_formula <- my_stat$formula_fn(
#'   outcome = 'ConvertedToAlzheimers',
#'   predictors = 'PLASMA_PTAU181_bl'
#' )
#'
#' my_model <- my_stat$fit_fn(
#'   formula = my_formula,
#'   data = adni_sample
#' )
aba_roc <- function(direction = '>',
                    methods = 'Youden',
                    tag.healthy = 0,
                    std.beta = FALSE,
                    complete.cases = TRUE,
                    extra.metrics = NULL) {

  fns <- list(
    'formula_fn' = aba_formula_roc,
    'fit_fn' = aba_fit_roc,
    'extra.metrics' = extra.metrics,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases,
      'include.basic' = FALSE
    ),
    'direction' = direction,
    'methods' = methods,
    'tag.healthy' = tag.healthy
  )
  fns$stat_type <- 'roc'
  class(fns) <- 'abaStat'
  return(fns)
}

aba_formula_roc <- function(outcome, predictors, covariates, ...) {
  if (length(predictors) > 1) stop('ROC predictors should only be length == 1.')
  predictor <- predictors[1]
  f <- as.character(glue('{predictor} ~ {outcome}'))
  return(f)
}

# fit a glm model
aba_fit_roc <- function(formula, data, ...) {
  model <- OptimalCutpoints::optimal.cutpoints(
    stats::formula(formula),
    data = data.frame(data),
    tag.healthy=0,
    direction='>',
    methods='Youden'
  )
  model$call$X <- stats::formula(formula)
  print(model)
  class(model) <- c('roc', class(model))
  return(model)
}

#' @export
aba_tidy.roc <- function(model, predictors, covariates, ...) {
  # coefficient is the cutoff value
  cut_val <- model$Youden$Global$optimal.cutoff$cutoff[1]
  x <- tibble::tibble(
    term = predictors,
    estimate = cut_val,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )
 return(x)
}


#' @export
aba_glance.roc <- function(x, x0, ...) {
  auc <- x$Youden$Global$measures.acc$AUC
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







