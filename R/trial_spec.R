#' Create a trial spec
#'
#' A trial spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - timepoints
#'   - stats
#'
#' @param groups vector. group selection criteria to use.
#' @param outcomes vector. outcomes to use.
#' @param time_var string time variable in data.
#' @param timepoints vector. timepoints to use.
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
                       time_var=NULL,
                       timepoints=NULL,
                       stats=NULL) {

  spec <- list(
    'groups' = groups,
    'outcomes' = outcomes,
    'timevar' = time_var,
    'timepoints' = timepoints,
    'stats' = stats
  )

  class(spec) <- 'trialSpec'

  return(
    spec
  )
}

#' Set the timepoints of a trial spec
#'
#' @param .model abaTrial
#' @param time_var variable or string. data variable with time values
#' @param ... numeric or character. how long the trial will be.
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_timepoints('VISIT', 1.5, 2)
set_timepoints <- function(.model, time_var, ...) {
  if (!is.null(.model$data)) {
    time_var <- colnames(
      .model$data %>%
        dplyr::select(rlang::enexpr(time_var))
    )[1]
  }
  .model$spec$time_var <- time_var
  .model$spec$timepoints <- c(...)
  .model
}


