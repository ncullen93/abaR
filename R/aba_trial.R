#' Create an aba trial
#'
#' An aba trial is composed of the following:
#'   - data: a data.frame to be used to fit the trial models
#'   - spec: the specification for the aba trial
#'   - fits: the fitted statistical models once `fit()` is called
#'
#' @param data data.frame. the data to use for the object
#' @param spec trialSpec. the spec to use for the model. Can be created with
#'   trial_spec()
#' @param results list the fitted statistical models
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model()
aba_trial <- function(data = NULL,
                      spec = trial_spec(),
                      results = list()) {

  m <- list(
    'data' = data,
    'spec' = spec,
    'results' = results
  )

  class(m) <- 'abaTrial'

  return(
    m
  )
}


#print.abaTrial <- function(x, ...) {
#  model <- x
#
#  #group_vals <- model$spec$group
#  #outcome_vals <- model$spec$outcomes
#  #covariate_vals <- model$spec$covariates
#  #predictor_vals <- model$spec$predictors[-1]
#  #stat_vals <- model$spec$stats
##
#  #cat('Groups:\n   ')
#  #cat(group_vals, sep='\n   ')
#  #cat('Outcomes:\n   ')
#  #cat(outcome_vals, sep='\n   ')
#  #cat('Covariates:\n   ', covariate_vals, '\n')
#  #cat('Predictors:\n   ')
#  #cat(predictor_vals, sep='\n   ')
#  #cat('Stats:\n   ')
#  #xx <- stat_vals %>% purrr::map_chr(~.$stat_type)
#  #cat(xx, sep='\n   ')
#}





