#' @importFrom generics fit
#' @export
generics::fit


#' Generic compile method
#'
#' @param model aba-type model
#'
#' @return aba-type model
#' @export
#'
#' @examples
#' 1 == 1
compile <- function(model) {
  UseMethod('compile')
}


#' Fit an aba model.
#'
#' This will trigger the fitting of all statistical models
#' (`stats`) on the different parameter combinations (`spec`).
#'
#' @param object abaModel. The aba model to be fitted.
#' @param ... additional parameters.
#'
#' @return abaModel
#' @export
#' @examples
#' m <- aba_model()
fit.abaModel <- function(object, ...) {
  model <- object

  # compile model
  model <- model %>% compile()

  # fit stats on spec
  fit_df <- model$results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      fits = parse_then_fit_abaModel(
        data=model$data,
        group=.data$groups,
        outcome=.data$outcomes,
        predictors=.data$predictors,
        covariates=.data$covariates,
        stats=.data$stats
      )
    ) %>%
    tidyr::unnest_wider(
      .data$fits
    )

  model$results <- fit_df
  return(model)
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
