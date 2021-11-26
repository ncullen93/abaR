

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
                    include.basic = TRUE,
                    extra.metrics = NULL) {
  fns <- list(
    'formula_fn' = aba_formula_std,
    'fit_fn' = aba_fit_glm,
    'extra.metrics' = extra.metrics,
    'params' = list(
      'std.beta' = std.beta,
      'complete.cases' = complete.cases,
      'include.basic' = include.basic
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
    data = data,
    na.action = na.omit
  )
  model$call$formula <- stats::formula(formula)
  return(model)
}

#' @export
aba_tidy.glm <- function(model, predictors, covariates, ...) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
}


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

  roc_obj <- pROC::roc(data, .Truth, .Predicted, ci = T, quiet = T)
  auc_val <- roc_obj$auc[1]
  auc_val_lo <- roc_obj$ci[1]
  auc_val_hi <- roc_obj$ci[3]

  # add other metrics here... sens, spec, ppv, npv, etc..
  # ...

  # Optimal Cutoff
  cut_model <- OptimalCutpoints::optimal.cutpoints(
    .Predicted ~ .Truth,
    data = data %>% data.frame(),
    tag.healthy=0, direction='<', methods='Youden'
  )
  cut_val <- cut_model$Youden$Global$optimal.cutoff$cutoff[1]

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

  # pivot longer to be like coefficients
  glance_df <- glance_df %>%
    pivot_longer(cols = everything()) %>%
    rename(term = name, estimate = value)

  # add confidence intervals
  glance_df <- glance_df %>%
    left_join(
      tibble::tibble(
        term = c('AUC'),
        conf.low = c(auc_val_lo),
        conf.high = c(auc_val_hi)
      ),
      by = 'term'
    )

  return(glance_df)
}


