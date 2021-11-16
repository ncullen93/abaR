#' Create a trial spec
#'
#' A trial spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - times
#'   - stats
#'
#' @param groups vector. group selection criteria to use.
#' @param stats character or vector. stat to use.
#' @param endpoints vector. endpoints
#' @param treatment character. treatment
#' @param covariates vector. covariates
#'
#' @return A trialSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_trial_spec()
aba_trial_spec <- function(groups=NULL,
                           endpoints=NULL,
                           treatment=NULL,
                           covariates=NULL,
                           stats=NULL) {

  spec <- list(
    'groups' = groups,
    'endpoints' = endpoints,
    'treatment' = treatment,
    'covariates' = covariates,
    'stats' = stats
  )

  class(spec) <- 'abaTrialSpec'

  return(
    spec
  )
}


#' Set the endpoints of an aba_trial spec
#'
#' @param .model abaTrial
#' @param ... endpoints
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_outcomes()
set_endpoints <- function(.model, ...) {
  .model$spec$endpoints <-
    unname(unlist(parse_select_expr(..., data=.model$data)))
  .model
}


#' Set the endpoints of an aba_trial spec
#'
#' @param .model abaTrial
#' @param ... endpoints
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_outcomes()
set_treatment <- function(.model, ...) {
  .model$spec$treatment <-
    unname(unlist(parse_select_expr(..., data=.model$data)))
  .model
}

