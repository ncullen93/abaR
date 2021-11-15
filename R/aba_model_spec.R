#' Create a model spec
#'
#' A model spec is composed of the following:
#'   - groups
#'   - outcomes
#'   - covariates
#'   - predictors
#'
#' @param groups vector. groups to use.
#' @param outcomes vector. outcomes to use.
#' @param covariates vector. covariates to use.
#' @param predictors vector or list. predictors to use
#' @param stats character or vector. stat to use.
#'
#' @return An abaModelSpec object
#'
#' @export
#'
#' @examples
#' spec <- aba_model_spec()
aba_model_spec <- function(groups='everyone()',
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

  class(spec) <- 'abaModelSpec'

  return(
    spec
  )
}


#' Set the groups of an model spec
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
  .model$spec$groups <-
    unname(unlist(parse_filter_expr(..., data=.model$data)))
  .model
}

#' Set the outcomes of an model spec
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
  .model$spec$outcomes <-
    unname(unlist(parse_select_expr(..., data=.model$data)))
  .model
}

#' Set the covariates of an model spec
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
  .model$spec$covariates <-
    unname(unlist(parse_select_expr(..., data=.model$data)))
  .model
}

#' Set the predictors of an model spec
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
  .model$spec$predictors <- unname(
    parse_select_expr(..., data=.model$data) %>%
    purrr::map_chr(~stringr::str_c(., collapse=' | '))
  )
  .model$spec$predictors <- c(
    '',
    .model$spec$predictors
  )
  .model
}

#' Set statistical model of an aba model
#'
#' @param .model abaModel. aba model to alter.
#' @param ... vector. Which statistical models to use.
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' # create default stat object by string
#' m <- aba_model() %>% set_stats('glm')
#' # create a stat object with - useful w/ extra params
#' m <- aba_model() %>% set_stats(aba_glm())
set_stats <- function(.model, ...) {
  stats <- list(...) %>%
    purrr::map(
      function(x) {
        if (is.character(x)) x <- aba_stat_lookup(x)
        return(x)
      }
    )
  names(stats) <- stats %>% purrr::map_chr('stat_type')
  .model$spec$stats <- stats
  .model
}

