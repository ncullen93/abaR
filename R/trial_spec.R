#' Create a trial spec
#'
#' A trial spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - times
#'   - stats
#'
#' @param groups vector. group selection criteria to use.
#' @param outcomes vector. outcomes to use.
#' @param times vector. times to use.
#' @param stats character or vector. stat to use.
#'
#' @return A trialSpec object
#'
#' @export
#'
#' @examples
#' spec <- trial_spec()
trial_spec <- function(groups=NULL,
                       outcomes=NULL,
                       times=NULL,
                       stats=NULL) {

  spec <- list(
    'groups' = groups,
    'outcomes' = outcomes,
    'times' = times,
    'stats' = stats
  )

  class(spec) <- 'trialSpec'

  return(
    spec
  )
}

#' Set the times of a trial spec
#'
#' @param .model abaTrial
#' @param ... numeric or character. how long the trial will be.
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- adni_sample %>% aba_trial() %>%
#'   set_times(VISIT == 1.5, VISIT==2)
set_times <- function(.model, ...) {
  .model$spec$times <-
    unname(unlist(parse_filter_expr(..., data=.model$data)))
  .model
}


