#' Create a trial spec
#'
#' A trial spec is composed of the following:
#'   - inclusion
#'   - outcomes
#'   - timepoints
#'   - stats
#'
#' @param inclusion vector. inclusion criteria to use.
#' @param outcomes vector. outcomes to use.
#' @param timepoints vector. timepoints to use.
#' @param stats character or vector. stat to use.
#'
#' @return A trialSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_spec()
trial_spec <- function(inclusion=NULL,
                       outcomes=NULL,
                       timepoints=NULL,
                       stats=NULL) {

  spec <- list(
    'inclusion' = inclusion,
    'outcomes' = outcomes,
    'timepoints' = timepoints,
    'stats' = stats
  )

  class(spec) <- 'trialSpec'

  return(
    spec
  )
}
