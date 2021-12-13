# take character/tidy inputs for SELECTION and turn to strings
parse_select_expr <- function(..., data) {
  rlang::enexprs(...) %>% purrr::map(
    function(xx) {
      if (is.null(data)) {
        if (is.character(xx)) {
          return(eval(xx))
        } else if (is.call(xx)) {
          xx <- tryCatch(
            {
              eval(xx, envir=new.env(parent=baseenv()))
            },
            error=function(cond) stop('You must set data if you are using tidy evaluation.')
          )
          return(xx)
        }
        else {
          stop('You must set data if you are using tidy evaluation.')
        }
      }
      # check for interaction term
      n_star <- 0
      if (is.character(xx)) {
        xx <- stringr::str_split(xx, '\\*')[[1]]
        n_star <- length(xx)
      }

      n <- names(tidyselect::eval_select(all_of(xx), data))

      if (n_star > 1) {
        n <- paste(n, collapse = ' * ')
      }
      n
    }
  )
}

# take character/tidy inputs for FILTERING and turn to strings
parse_filter_expr <- function(..., data) {

  rlang::enexprs(...) %>% purrr::map(
    function(x) {
      if (is.character(x)) {
        #if (x == 'everyone()') x <- 'TRUE'
        x <- str2lang(x)
        if (is.null(data)) return(deparse(x))
      } else {
        # parse list of statements: e.g. list(DX_bl == 'CU', AGE_bl < 85)
        if (stringr::str_starts(deparse(x, width.cutoff=500L), 'list\\(')) {
          x <- stringr::str_replace_all(
            deparse(x,  width.cutoff=500L),
            c('list\\(' = '', '\\)' = '', ',' = ' &')
          )
          x <- str2lang(x)
        }

        if (is.null(data)) stop('You must set data if you are using tidy evaluation.')
      }
      # check that filter works
      data_tmp <- data %>% dplyr::filter(!!x)
      # return string version of filter
      deparse(x, width.cutoff=500L)
    }
  )
}


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
# e.g. aba_stat_lookup('glm') is equivalent to stat_glm()
# but this function happens behind the scenes
aba_stat_lookup <- function(stat) {
  if (is.character(stat)) {
    stat_fn <- methods::getFunction(glue::glue('stat_{stat}'))
    stat <- stat_fn()
  }
  return(stat)
}

# generic for internal utility function aba_glance
aba_glance <- function(x, ...) {
  UseMethod('aba_glance')
}

# generic for internal utility function aba_tidy
aba_tidy <- function(model, predictors, covariates, ...) {
  UseMethod('aba_tidy')
}

# generic for internal utility function emmeans
run_emmeans <- function(fit, extra_params) {
  UseMethod('run_emmeans')
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

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

