#' Create a power stat
#'
#' @param n integer. sample size
#' @param delta float. treatment effect between 0 - 1
#' @param power float. power between 0 - 1
#' @param alpha float. alpha value
#' @param method string. method used to calculate power
#'
#' @return abaStat object
#' @export
#'
#' @examples
#' p <- stat_power(alpha=0.05, power=0.8, delta=0.3, method='t.test')
stat_power <- function(n=NULL,
                       delta=NULL,
                       power=NULL,
                       alpha=0.05,
                       method=c('t.test', 'lme')) {
  if (is.null(n) + is.null(delta) + is.null(power) > 1) {
    stop('Only one of `n`, `delta`, and `power` can be null.')
  }
  method = match.arg(method)

  fns <- list(
    params = list(
      n = n,
      delta = delta,
      power = power,
      alpha = alpha,
      method = method
    )
  )
  fns$stat_type <- 'power'
  class(fns) <- 'abaStat'
  return(fns)
}
