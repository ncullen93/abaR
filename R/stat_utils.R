
# function to make a standard formula using by glm, lm, etc
standard_formula_fn <- function(outcome, predictors, covariates, ...) {
  f <- paste0(outcome, " ~ ")
  if (length(covariates) > 0) {
    f <- paste0(f, paste(covariates, collapse = " + "))
    if (length(predictors) > 0) f <- paste0(f, ' + ')
  }
  if (length(predictors) > 0) f <- paste0(f, paste(predictors, collapse = " + "))
  if (length(covariates) + length(predictors) == 0) f <- paste0(f, '1')
  return(f)
}

aba_stat_lookup <- function(stat) {
  if (is.character(stat)) {
    stat_fn <- methods::getFunction(glue::glue('aba_{stat}'))
    stat <- stat_fn()
  }
  return(stat)
}
