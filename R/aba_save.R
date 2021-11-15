

#' Save an ABA object to file
#'
#' @param object ABA object
#' @param file filename
#' @param ... other params
#'
#' @return NA
#' @export
#'
#' @examples
#' x <- 1
aba_save <- function(object, file, include_data = TRUE, include_fit = TRUE) {
  if (!include_data & ('abaModel' %in% class(object))) {
    object$data <- NULL
  }
  if (!include_fit & ('abaModel' %in% class(object))) {
    stat_names <- names(object$spec$stats)
    object$results[,stat_names] <- NULL
  }
  saveRDS(object = object, file = file)
}


#' Load an ABA object from file
#'
#' @param file ABA object
#' @param ... filename
#'
#' @return NA
#' @export
#'
#' @examples
#' x <- 1
aba_load <- function(file, ...) {
  readRDS(file = file)
}
