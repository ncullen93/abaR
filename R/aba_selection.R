#' Cfreate an aba selection object
#'
#' @param model model
#' @param method method
#' @param criteria criteria
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_selection <- function(model,
                          method = c('forward', 'backward'),
                          criteria = c('aic', 'pval'),
                          threshold = NULL,
                          verbose = TRUE) {

  method <- match.arg(method)
  criteria <- match.arg(criteria)

  if ((criteria == 'aic') & is.null(threshold)) threshold <- 2
  if ((criteria == 'pval') & is.null(threshold)) threshold <- 0.1

  m <- list(
    'model' = model,
    'method' = method,
    'criteria' = criteria,
    'threshold' = threshold,
    'verbose' = verbose
  )
  class(m) <- 'abaSelection'
  return(m)
}
