

#' Create a glm stat to use for an aba model.
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
#' my_stat <- aba_glm()
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
aba_glm <- function(std.beta = FALSE,
                    complete.cases = TRUE,
                    extra.metrics = NULL) {
  fns <- list(
    'formula_fn' = aba_formula_std,
    'fit_fn' = aba_fit_glm,
    'extra.metrics' = extra.metrics,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases
    )
  )
  fns$stat_type <- 'glm'
  class(fns) <- 'abaStat'
  return(fns)
}

# fit a glm model
aba_fit_glm <- function(formula, data, ...) {
  model <- stats::glm(
    stats::formula(formula),
    family = 'binomial',
    data = data
  )
  model$call$formula <- stats::formula(formula)
  return(model)
}

#aba_tidy.glm <- function(x, ...) {}

#' @export
aba_glance.glm <- function(x, x0, ...) {
  # tidy glance
  glance_df <- broom::glance(x)

  # custom glance
  fit <- x
  fit0 <- x0
  data <- stats::model.frame(fit) %>% tibble::tibble()
  outcome <- colnames(data)[1]

  data <- data %>%
    dplyr::mutate(
      .Predicted = stats::predict(fit, type='response'),
      .Truth = factor(.data[[outcome]]) # probably should do this before fitting
    )

  auc_val <- yardstick::roc_auc(
    data,
    '.Truth',
    '.Predicted',
    event_level = 'second'
  )[['.estimate']]

  # add other metrics here... sens, spec, ppv, npv, etc..
  # ...

  # Optimal Cutoff
  cut_model <- OptimalCutpoints::optimal.cutpoints(
    .Predicted ~ .Truth,
    data = data %>% data.frame(),
    tag.healthy=0, direction='<', methods='Youden'
  )
  cut_val <- cut_model$Youden$Global$optimal.cutoff$cutoff

  # compare current model to null model
  s <- stats::anova(fit, fit0)
  null_pval <- 1 - stats::pchisq(abs(s$Deviance[2]), abs(s$Df[2]))

  # combine broom::glance with extra metrics
  glance_df <- glance_df %>%
    dplyr::bind_cols(
      tibble::tibble(
        AUC = auc_val,
        Cut = cut_val,
        Pval = null_pval
      )
    )
  return(glance_df)
}


