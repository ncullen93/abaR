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
#' spec <- trial_spec()
trial_spec <- function(inclusion=NULL,
                       outcomes=NULL,
                       time_var=NULL,
                       timepoints=NULL,
                       stats=NULL) {

  spec <- list(
    'inclusion' = inclusion,
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


#' Set the inclusion of a trial spec
#'
#' @param .model abaTrial
#' @param ... statements. inclusion criteria
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_inclusion()
set_inclusion <- function(.model, ...) {
  .model$spec$inclusion <-
    unname(unlist(parse_filter_expr(..., data=.model$data)))
  .model
}

#' Set the timepoints of a trial spec
#'
#' @param .model abaTrial
#' @param ... numeric or character. how long the trial will be.
#'
#' @return An abaTrial object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_timepoints()
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


