#' Create an aba spec
#'
#' An aba spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - covariates
#'   - predictors
#'
#' @param groups vector. groups to use.
#' @param outcomes vector. outcomes to use.
#' @param covariates vector. covariates to use.
#' @param predictors vector or list. predictors to use
#' @param stats character or vector. stats to use.
#'
#' @return An abaSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_spec()
aba_spec <- function(groups=NULL,
                     outcomes=NULL,
                     covariates=NULL,
                     predictors=NULL,
                     stats=NULL) {

  spec <- list(
    'groups' = groups,
    'outcomes' = outcomes,
    'covariates' = covariates,
    'predictors' = predictors,
    'stats' = stats
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
#' @return An abaModel object
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
  .model[['spec']][['groups']] <- list(...)
  .model
}

#' Set the outcomes of an aba spec
#'
#' @param .model abaModel
#' @param ... outcomes
#'
#' @return An abaModel object
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
#' @return An abaModel object
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
#' @return An abaModel object
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


#' Set statistical model of an aba model
#'
#' @param model abaModel. aba model to alter.
#' @param stat string. which statistical model to use.
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_stats('glm')
set_stats <- function(model, stat) {
  UseMethod('set_stats')
}

#' @export
set_stats.abaModel <- function(model, ...) {
  model[['spec']][['stats']] <- list(...)
  model
}
