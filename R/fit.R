#' Compile the spec of an abaModel to ensure it is consistent with the
#' data that the abModel has. This also creates a tibble with all the
#' different combinations that will be tested ('fits')
#'
#' @param model abaModel. The model to compile.
#'
#' @return abaModel
#' @export
#'
#' @examples
#' m <- aba_model()
compile <- function(model) {
  if (is.null(model$data)) stop('You must set data first.')

  data <- model$data

  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors
  stat_vals <- model$spec$stats

  val_list <- list(
    'groups' = group_vals,
    'outcomes' = outcome_vals,
    'predictors' = predictor_vals,
    'covariates' = stringr::str_c(covariate_vals, collapse='_+_'),
    'stats' = stringr::str_c(stat_vals, collapse='_+_')
  )

  init_df <- val_list %>% purrr::cross_df()
  model$fits$init_df <- init_df
  model
}
