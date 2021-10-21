#' Create an aba spec
#'
#' An aba spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - covariates
#'   - predictors
#'
#' @param groups vector. groups to use.
#' @param outcomes vector. outcomes to use.
#' @param covariates vector. covariates to use.
#' @param predictors vector or list. predictors to use
#' @param stats character or vector. stats to use.
#'
#' @return An abaSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_spec()
aba_spec <- function(groups=NULL,
                     outcomes=NULL,
                     covariates=NULL,
                     predictors=NULL,
                     stats=NULL) {

  spec <- list(
    'groups' = groups,
    'outcomes' = outcomes,
    'covariates' = covariates,
    'predictors' = predictors,
    'stats' = stats
  )

  class(spec) <- 'abaSpec'

  return(
    spec
  )
}

# take characters or tidy evaluation inputs and turn to strings
parse_select_expr <- function(..., data) {


  #rlang::enquos(...) %>% purrr::map(
  #  function(x) {
  #    data %>% select(!!x) %>% colnames()
  #  }
  #)
  rlang::enexprs(...) %>% purrr::map(
    function(x) {
      if (is.character(x)) {
        x <- str2lang(x)
        if (is.null(data)) return(x)
      } else if (is.call(x)){
        if (is.null(data)) return(eval(x))
      } else {
        if (is.null(data)) stop('You must set data first when using tidy evaluation.')
      }
      # check that filter works
      data %>% dplyr::select(!!x) %>% colnames()
    }
  )
}

parse_filter_expr <- function(..., data) {

  rlang::enexprs(...) %>% purrr::map(
    function(x) {
      if (is.character(x)) {
        x <- str2lang(x)
        if (is.null(data)) return(x)
      } else {
        if (is.null(data)) stop('You must set data if you are using tidy evaluation.')
      }
      # check that filter works
      data_tmp <- data %>% dplyr::filter(!!x)
      # return string version of filter
      deparse(x)
    }
  )
}

#' Set the groups of an aba spec
#'
#' @param .model abaModel
#' @param ... groups
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_groups()
set_groups <- function(.model, ...) {
  UseMethod('set_groups')
}

#' @export
set_groups.abaModel <- function(.model, ...) {
  .model[['spec']][['groups']] <- unlist(parse_filter_expr(..., data=.model$data))
  .model
}

#' Set the outcomes of an aba spec
#'
#' @param .model abaModel
#' @param ... outcomes
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_outcomes()
set_outcomes <- function(.model, ...) {
  UseMethod('set_outcomes')
}

#' @export
set_outcomes.abaModel <- function(.model, ...) {
  .model[['spec']][['outcomes']] <- unlist(parse_select_expr(..., data=.model$data))
  .model
}

#' Set the covariates of an aba spec
#'
#' @param .model abaModel
#' @param ... covariates
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_covariates()
set_covariates <- function(.model, ...) {
  UseMethod('set_covariates')
}

#' @export
set_covariates.abaModel <- function(.model, ...) {
  .model[['spec']][['covariates']] <- unlist(parse_select_expr(..., data=.model$data))
  .model
}

#' Set the predictors of an aba spec
#'
#' @param .model abaModel
#' @param ... predictors
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_predictors()
set_predictors <- function(.model, ...) {
  UseMethod('set_predictors')
}

#' @export
set_predictors.abaModel <- function(.model, ...) {
  .model[['spec']][['predictors']] <-
    parse_select_expr(..., data=.model$data) %>%
    purrr::map_chr(~stringr::str_c(., collapse='_+_'))
  .model
}

#' Set statistical model of an aba model
#'
#' @param model abaModel. aba model to alter.
#' @param ... vector. Which statistical models to use.
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_stats('glm')
set_stats <- function(model, ...) {
  UseMethod('set_stats')
}

#' @export
set_stats.abaModel <- function(model, ...) {
  model[['spec']][['stats']] <- c(...)
  model
}
