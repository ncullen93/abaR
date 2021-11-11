
# function to make a standard formula using by glm, lm, etc
aba_formula_std <- function(outcome, predictors, covariates, ...) {
  f <- paste0(outcome, " ~ ")
  if (length(covariates) > 0) {
    f <- paste0(f, paste(covariates, collapse = " + "))
    if (length(predictors) > 0) f <- paste0(f, ' + ')
  }
  if (length(predictors) > 0) f <- paste0(f, paste(predictors, collapse = " + "))
  if (length(covariates) + length(predictors) == 0) f <- paste0(f, '1')
  return(f)
}

# lookup abaStat object/function from a string supplied by user
# e.g. aba_stat_lookup('glm') is equivalent to aba_glm()
# but this function happens behind the scenes
aba_stat_lookup <- function(stat) {
  if (is.character(stat)) {
    stat_fn <- methods::getFunction(glue::glue('aba_{stat}'))
    stat <- stat_fn()
  }
  return(stat)
}

#' aba glance generic
#'
#' @param x model
#'
#' @param ... extra parameters
#'
#' @export
aba_glance <- function(x, ...) {
  UseMethod('aba_glance')
}

#' @export
print.abaStat <- function(x, ...) {
  cat(x$stat_type)
  if (!is.null(x$extra_params)) {
    cat('(')
    ep <- x$extra_params
    for (ix in seq_along(ep)) {
      cat(names(ep)[ix], ' = ', ep[[ix]], sep='')
      if (ix != length(ep)) cat(' | ')
    }
    cat(')')
  }
}
