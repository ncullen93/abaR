#' Set data element of an aba model
#'
#' @param model abaModel. model to which data will be added
#' @param data data.frames. data to add
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_data(data.frame(x=c(1,2,3)))
set_data <- function(model, data) {
  UseMethod('set_data')
}

#' @export
set_data.abaModel <- function(model, data) {
  if (!is.data.frame(data)) stop('data argument must be data.frame')

  model$data <- data
  model
}

#' @export
set_data.abaTrial <- function(model, data) {
  if (!is.data.frame(data)) stop('data argument must be data.frame')

  model$data <- data
  model
}
