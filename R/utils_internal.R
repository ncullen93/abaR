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
      is_multi <- FALSE
      if (is.character(x)) {
        #if (x == 'everyone()') x <- 'TRUE'
        x <- str2lang(x)
        if (is.null(data)) return(deparse(x))
      } else {
        if (startsWith(deparse(x), 'all_levels')) {
          x <- deparse(x)

          var <- gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
          vars <- strsplit(var, ',')[[1]] %>% purrr::map_chr(trimws)

          # get all different values of each variable
          group_vals <- vars %>% purrr::map(
            function(v) {
              vals <- unique(data[[v]])
              vals <- vals[!is.na(vals) & vals != '']
              xx <- paste0(v, ' == ', "'",as.character(vals), "'")
              xx <- xx %>% map_chr(~paste0('"',.,'"'))
              xx
            }
          )
          x <- group_vals %>%
            cross() %>%
            map(
              function(v) {
                xx <- paste0('(',v,')',collapse=' & ')
                xx <- stringr::str_replace_all(xx, '\\"','')
                xx <- stringr::str_replace_all(xx, "'","\\'")
                #print(xx)
                xx
              }
            )
          x_orig <- x
          # now combine them together
          x <- x %>% map(str2lang)
          #x_orig <- x
          #print(x)
          is_multi <- TRUE
        }

        # parse list of statements: e.g. list(DX_bl == 'CU', AGE_bl < 85)
        if (stringr::str_starts(deparse(x, width.cutoff=500L), 'list\\(')) {
          x <- stringr::str_replace_all(
            deparse(x,  width.cutoff=500L),
            c('list'='', '\\(' = '', '\\)' = '', ',' = ' &')
          )
          x <- str2lang(x)
        }

        if (is.null(data)) stop('You must set data if you are using tidy evaluation.')
      }

      data_tmp <- data %>% dplyr::filter(!!x)
      # return string version of filter
      xx <- deparse(x, width.cutoff=500L)
      if (is_multi) xx <- x_orig
      xx
    }
  )
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

empty_tidy_data <- function() {
  tibble::tibble(
    term = NA,
    estimate = NA,
    std.error = NA,
    statistic  = NA,
    p.value  = NA,
    conf.low  = NA,
    conf.high = NA
  )
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

