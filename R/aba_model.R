#' Create an aba model
#'
#' An aba model is composed of the following:
#'   - data: a data.frame to be used to fit the statistical models
#'   - spec: the specification for the aba model composed of the following:
#'     - groups: which parts of the data to fit separate models on
#'     - outcomes: which variables to use as dependent variables
#'     - covariates: which variables to use as fixed independent variables in
#'         every single model that is fit
#'     - predictors: which variables to use as independent variables, but never
#'         together in the same model
#'   - fits: the fitted statistical models once `aba_fit()` is called
#'
#' @param data data.frame the data to use for the object
#' @param spec abaModelSpec the spec to use for the model. Can be created with
#'   model_spec().
#' @param results list the fitted statistical models
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model()
aba_model <- function(data = NULL,
                      spec = aba_model_spec(),
                      verbose = FALSE) {

  m <- list(
    'data' = data,
    'spec' = spec,
    'results' = list(),
    'verbose' = verbose
  )

  class(m) <- 'abaModel'

  return(
    m
  )
}

#' @export
print.abaModel <- function(x, ...) {
  model <- x

  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  outcome_labels <- names(model$spec$outcomes)
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors[-1]
  predictor_labels <- names(model$spec$predictors[-1])
  stat_vals <- model$spec$stats


  cat('\n')
  cat('Groups:\n   ')
  cat(group_vals, sep='\n   ')

  # OUTCOMES #
  cat('\nOutcomes:\n   ')
  if (!is.null(outcome_labels)) {
    cat(paste0(outcome_labels, ' (', outcome_vals, ')'), sep='\n   ')
  } else {
    cat(outcome_vals, sep='\n   ')
  }

  # COVARIATES #
  cat('\nCovariates:\n   ', covariate_vals, '\n')

  # PREDICTORS #
  if (!is.null(model$spec$treatment)) {
    cat('\nTreatment:\n   ', model$spec$treatment, '\n')
  } else {
    cat('\nPredictors:\n   ')
    if (!is.null(predictor_labels)) {
      cat(paste0(predictor_labels, ': ', predictor_vals), sep='\n   ')
    } else {
      cat(predictor_vals, sep='\n   ')
    }
  }
  cat('\nStats:\n   ')
  stat_vals %>% purrr::walk(~cat(print(.),'\n   '))
}



