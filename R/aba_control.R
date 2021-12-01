#' Create an aba control which determines how an aba summary will be printed.
#'
#' @param covars boolean. Whether to include covariates summary.
#'
#' @return
#' @export
#'
#' @examples
#' ctrl <- aba_control()
aba_control <- function(include_covariates = TRUE,
                        include_intercept = FALSE) {

  ctrl <- list(
    include_covariates = include_covariates,
    include_intercept = include_intercept
  )

  return(ctrl)
}
