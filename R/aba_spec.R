#' Create an aba spec
#'
#' An aba spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - covariates
#'   - predictors
#'
#' @return
#' An abaSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_spec()
aba_spec <- function() {

  spec <- list(
    'groups' = NULL,
    'outcomes' = NULL,
    'covariates' = NULL,
    'predictors' = NULL
  )

  class(spec) <- 'abaSpec'

  return(
    spec
  )
}

#' Set the groups of an aba spec
#'
#' @param .model abaModel
#' @param ... groups
#'
#' @return
#' An abaModel with groups altered
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_groups()
set_groups <- function(.model, ...) {
  UseMethod('set_groups')
}

#' @export
set_groups.abaModel <- function(.model, ...) {
  .model[['spec']][['groups']] <- c(...)
  .model
}

#' Set the outcomes of an aba spec
#'
#' @param .model abaModel
#' @param ... outcomes
#'
#' @return
#' An abaModel with outcomes altered
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_outcomes()
set_outcomes <- function(.model, ...) {
  UseMethod('set_outcomes')
}

#' @export
set_outcomes.abaModel <- function(.model, ...) {
  .model[['spec']][['outcomes']] <- c(...)
  .model
}

#' Set the covariates of an aba spec
#'
#' @param .model abaModel
#' @param ... covariates
#'
#' @return
#' An abaModel with covariates altered
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_covariates()
set_covariates <- function(.model, ...) {
  UseMethod('set_covariates')
}

#' @export
set_covariates.abaModel <- function(.model, ...) {
  .model[['spec']][['covariates']] <- c(...)
  .model
}

#' Set the predictors of an aba spec
#'
#' @param .model abaModel
#' @param ... predictors
#'
#' @return
#' An abaModel with predictors altered
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_predictors()
set_predictors <- function(.model, ...) {
  UseMethod('set_predictors')
}

#' @export
set_predictors.abaModel <- function(.model, ...) {
  .model[['spec']][['predictors']] <- list(...)
  .model
}
