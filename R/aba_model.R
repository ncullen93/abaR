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

# compile abaModel
#' @export
compile.abaModel <- function(model) {
  data <- model$data
  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors
  stat_vals <- model$spec$stats

  if (is.null(predictor_vals)) predictor_vals <- ""

  # check that minimum parameters have been set
  if (is.null(model$data)) stop('You must set data before fitting.')
  if (length(outcome_vals) == 0) stop('You must set at least one outcome.')
  if (length(predictor_vals) + length(covariate_vals) == 0) {
    stop('You must set at least one predictor or one covariate')
  }
  if (length(stat_vals) == 0) stop('You must set at least one stat.')

  val_list <- list(
    'groups' = group_vals,
    'outcomes' = as.vector(outcome_vals),
    'predictors' = as.vector(predictor_vals),
    'covariates' = stringr::str_c(covariate_vals, collapse=' | '),
    'stats' = list(stat_vals)
  )

  init_df <- val_list %>% purrr::cross_df()
  init_df <- cbind(MID = stringr::str_c('M', rownames(init_df)), init_df)
  model$results <- init_df %>% dplyr::tibble()
  return(model)
}

# need a preprocessing function to parse

parse_then_fit_abaModel <- function(
  data, group, outcome, predictors, covariates, stats
) {

  # filter original data by group
  my_data <- data %>% dplyr::filter(
    rlang::eval_tidy(rlang::parse_expr(group))
  )

  # parse predictors and covariates into vectors
  predictors <- unlist(strsplit(predictors,' \\| '))
  covariates <- unlist(strsplit(covariates,' \\| '))

  # lookup stat objects from strings
  # fit the models
  stat_models <- stats %>%
    purrr::map(
      function(stat_obj) {
        extra_params <- stat_obj$extra_params
        my_formula <- stat_obj$formula_fn(
          outcome, predictors, covariates, extra_params
        )
        my_model <- stat_obj$fit_fn(
          my_formula, my_data, extra_params
        )
        return(my_model)
      }
    )

  return(
    list(stat_models)
  )
}

#' @export
print.abaModel <- function(x, ...) {
  model <- x

  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors[-1]
  stat_vals <- model$spec$stats

  cat('Groups:\n   ')
  cat(group_vals, sep='\n   ')
  cat('Outcomes:\n   ')
  cat(outcome_vals, sep='\n   ')
  cat('Covariates:\n   ', covariate_vals, '\n')
  cat('Predictors:\n   ')
  cat(predictor_vals, sep='\n   ')
  cat('Stats:\n   ')
  stat_vals %>% purrr::walk(~cat(print(.),'\n   '))
}



