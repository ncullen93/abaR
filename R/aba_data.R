#' Set data element of an aba model
#'
#' @param .model abaModel. model to which data will be added
#' @param ... data.frames. data to add
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_data(data.frame(x=c(1,2,3)))
set_data <- function(.model, ...) {
  UseMethod('set_data')
}

#' @export
set_data.abaModel <- function(.model, ...) {
  inputs <- list(...)
  sum_nondf <- inputs %>% purrr::map_lgl(~!is.data.frame(.)) %>% sum()
  if (sum_nondf > 0) stop('arguments should only be data.frames')

  .model[['data']] <- list(...)
  .model
}
