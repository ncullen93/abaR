
# function to make a standard formula using by glm, lm, etc
formula_std <- function(outcome, predictors, covariates, ...) {
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

#' aba tidy generic
#'
#' @param model model
#' @param predictors predictors
#' @param covariates covariates
#' @param ... extra parameters
#'
#' @export
aba_tidy <- function(model, predictors, covariates, ...) {
  UseMethod('aba_tidy')
}


#' Run emmeans on a model
#'
#' @param model model
#' @param treatment treatment
#' @param stats_obj stats obj
#' @param ... extra parameters
#'
#' @return
#' @export
#'
#' @examples
#' x <- 1
aba_emmeans <- function(model, treatment, stats_obj, ...) {
  UseMethod('aba_emmeans')
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

# std normalize a dataframe
normalize_data <- function(data,
                           outcome,
                           predictors,
                           covariates,
                           stat_obj) {
  std.beta <- stat_obj$params$std.beta
  complete.cases <- stat_obj$params$complete.cases

  all_vars <- c(covariates, predictors)

  if (std.beta) {
    factor_vars <- sapply(data[,all_vars], class)
    factor_vars <- names(factor_vars[factor_vars=='factor'])
    scale_vars <- all_vars[!(all_vars %in% factor_vars)]
    data[, scale_vars] <- scale(data[, scale_vars])
  }
  if (complete.cases) {
    data <- data[complete.cases(data[, all_vars]), ]
  }

  data
}



