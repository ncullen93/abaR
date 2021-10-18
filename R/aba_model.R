


#' Create an aba model
#'
#' @return abaModel
#' @export
#'
#' @examples
#' m <- aba_model()
aba_model <- function() {

  m <- list(
    'data' = list(),
    'spec' = list(),
    'fits' = list()
  )

  class(m) <- 'abaModel'

  return(
    m
  )
}
