

#' Write an ABA object to file
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
aba_write <- function(object, file, name = NULL, include_data = TRUE, include_fit = TRUE) {
  if (!include_data & ('abaModel' %in% class(object))) {
    object$data <- NULL
  }
  if (!include_fit & ('abaModel' %in% class(object))) {
    stat_names <- names(object$spec$stats)
    object$results[,stat_names] <- NULL
  }

  if ('abaBoard' %in% class(file)) {
    board <- file$board
    if (is.null(name)) name <- 'abaModel'
    board %>% pin_write(object, name = name, type = 'rds')
  } else {

    saveRDS(object = object, file = file)
  }

}


#' Read an ABA object from file
#'
#' @param file ABA object
#' @param ... filename
#'
#' @return NA
#' @export
#'
#' @examples
#' x <- 1
aba_read <- function(file, name = NULL, ...) {
  if ('abaBoard' %in% class(file)) {
    if (is.null(name)) stop('Must give name to read from an ABA board.')
    board <- file$board
    object <- board %>% pin_read(name)
  } else {
    object <- readRDS(file = file)
  }
  return(object)
}


aba_board <- function(path, ...) {
  pin_board <- board_folder(path = '/Users/ni5875cu/Dropbox/aba_test/')
  my_board <- list(
    'board' = pin_board
  )
  class(my_board) <- 'abaBoard'

  return(my_board)
}


