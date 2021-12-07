

#' Create a demographics table from a fitted aba object
#'
#' @param object aba object
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_demographics <- function(model,
                     strata = NULL,
                     include_predictors = TRUE,
                     include_covariates = TRUE,
                     include_outcomes = TRUE,
                     add_vars = NULL,
                     data_filter = NULL,
                     ...) {
  UseMethod('aba_demographics')
}

#' @export
aba_demographics.abaSummary <- function(model,
                                strata = NULL,
                                include_predictors = TRUE,
                                include_covariates = TRUE,
                                include_outcomes = TRUE,
                                add_vars = NULL,
                                data_filter = NULL,
                                ...) {
  model$model %>% aba_demo(
    strata = strata,
    include_predictors = include_predictors,
    include_covariates = include_covariates,
    include_outcomes = include_outcomes,
    add_vars = add_vars,
    data_filter = data_filter,
    ...
  )
}

#' @export
aba_demographics.abaModel <- function(model,
                              strata = NULL,
                              include_predictors = TRUE,
                              include_covariates = TRUE,
                              include_outcomes = TRUE,
                              add_vars = NULL,
                              data_filter = NULL,
                              ...) {
  data <- model$data
  if (!is.null(data_filter)){
    data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(data_filter)))
  }

  predictors <- model %>% get_predictors()
  covariates <- model$spec$covariates
  outcomes <- model$spec$outcomes

  all_vars <- c()
  if (include_outcomes) all_vars <- c(all_vars, outcomes)
  if (include_predictors) all_vars <- c(all_vars, predictors)
  if (include_covariates) all_vars <- c(all_vars, covariates)

  if (!is.null(add_vars)) all_vars <- c(all_vars, add_vars)

  factor_vars <- all_vars[
    all_vars %>% purrr::map_lgl(~class(data[[.]]) %in% c('character', 'factor'))
  ]

  # TODO...
  # check if time variable is present in any stats
  # describe only baseline if longitudinal data is used

  if (is.null(strata)) {
    tbl <- tableone::CreateTableOne(
      vars = all_vars,
      factorVars = factor_vars,
      data = data,
      test = T, includeNA = T, addOverall = T
    )
  } else {
    tbl <- tableone::CreateTableOne(
      vars = all_vars,
      factorVars = factor_vars,
      data = data,
      strata = strata,
      test = T, includeNA = T, addOverall = T
    )
  }

  return(tbl)
}
