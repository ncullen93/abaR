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
#'   - fits: the fitted statistical models once `fit()` is called
#'
#' @param data data.frame the data to use for the object
#' @param spec abaSpec the spec to use for the model. Can be created with
#'   aba_spec().
#' @param results list the fitted statistical models
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model()
aba_model <- function(data = NULL,
                      spec = aba_spec(),
                      results = list()) {

  m <- list(
    'data' = data,
    'spec' = spec,
    'results' = results
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
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors
  stat_vals <- model$spec$stats
  cat('Groups:\n   ')
  cat(group_vals, sep='\n   ')
  cat('Outcomes:\n   ')
  cat(outcome_vals, sep='\n   ')
  cat('Covariates:\n   ', covariate_vals, '\n')
  cat('Predictors:\n   ')
  cat(predictor_vals, sep='\n   ')
  cat('Stats:\n   ', stat_vals)
  if (length(stat_vals) > 0) {
    if (length(model$results) == 0) {
      cat(' (not fit)')
    } else {
      cat(' (fit)')
    }
  }

  cat('\n')



  #parameters <- x$spec
  #for (group in parameters$groups) {
  #  for (outcome in parameters$outcomes) {
  #    for (predictor in parameters$predictors) {
  #      cat(":\n")
  #      line1 <- paste0(
  #        "Group: ", group, "\n",
  #        "Outcome: ", outcome, "\n",
  #        "Predictors: ", stringr::str_replace_all(predictor, '\\_\\+\\_', ' + '),
  #      )
  #      #res.df <- x[["models"]][[group]][[outcome]][[predictor]]
#
  #      cat(paste0(line1, "\n"))
  #      cat('Fit:\n')
  #      if (x$is_fit) {
  #        r <- x$results %>%
  #          filter(
  #            groups == group,
  #            outcomes == outcome,
  #            predictors == predictor
  #          )
  #        cat()
  #      }
  #      cat("---\n")
  #      # cat('\n')
  #    }
  #  }
  #}
}





