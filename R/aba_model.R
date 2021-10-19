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
#' @param fits list the fitted statistical models
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model()
aba_model <- function(data = NULL,
                      spec = aba_spec(),
                      fits = list()) {

  m <- list(
    'data' = data,
    'spec' = spec,
    'fits' = fits
  )

  class(m) <- 'abaModel'

  return(
    m
  )
}


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
  model
}


