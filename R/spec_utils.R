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
      names(tidyselect::eval_select(xx, data))
    }
  )
}

# take character/tidy inputs for FILTERING and turn to strings
parse_filter_expr <- function(..., data) {

  rlang::enexprs(...) %>% purrr::map(
    function(x) {
      if (is.character(x)) {
        x <- str2lang(x)
        if (is.null(data)) return(deparse(x))
      } else {
        # parse list of statements: e.g. list(DX_bl == 'CU', AGE_bl < 85)
        if (stringr::str_starts(deparse(x), 'list\\(')) {
          x <- stringr::str_replace_all(
            deparse(x),
            c('list\\(' = '', '\\)' = '', ',' = ' &')
          )
          x <- str2lang(x)
        }
        if (is.null(data)) stop('You must set data if you are using tidy evaluation.')
      }
      # check that filter works
      data_tmp <- data %>% dplyr::filter(!!x)
      # return string version of filter
      deparse(x)
    }
  )
}

# used to include all rows of dataset (no filtering)
# e.g. aba_model() %>% set_groups(DX_bl=='CU', everyone())
everyone <- function() {
  TRUE
}
