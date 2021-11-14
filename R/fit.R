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


