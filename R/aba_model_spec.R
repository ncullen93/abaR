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
#' @param labels vector optional labels for spec parameter - printing/plotting.
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_groups()
set_groups <- function(.model, ..., labels = NULL) {
  .model$spec$groups <-
    unname(unlist(parse_filter_expr(..., data=.model$data)))

  # set labels
  if (!is.null(labels)) {
    names(.model$spec$groups) <- labels
  }

  .model
}

#' Set the outcomes of an model spec
#'
#' @param .model abaModel
#' @param ... outcomes
#' @param labels vector optional labels for spec parameter - printing/plotting.
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_outcomes()
set_outcomes <- function(.model, ..., labels = NULL) {
  .model$spec$outcomes <-
    unname(unlist(parse_select_expr(..., data=.model$data)))

  # set labels
  if (!is.null(labels)) {
    names(.model$spec$outcomes) <- labels
  }

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
#' @param labels vector optional labels for spec parameter - printing/plotting.
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_predictors()
set_predictors <- function(.model,
                           ...,
                           labels = NULL) {
  .model <-
    tryCatch(
      {
        # expect not a list input
        .model$spec$predictors <- unname(
          parse_select_expr(..., data=.model$data) %>%
            purrr::map_chr(~stringr::str_c(., collapse=' | '))
        )
        .model$spec$predictors <- c(
          '',
          .model$spec$predictors
        )
        if (!is.null(labels)) {
          names(.model$spec$predictors) <- c('Basic', labels)
        }
        return(.model)
      },
      error = function(cond) {
        # try with expectation of list input
        predictors <- list(...)[[1]]
        predictor_labels <- names(predictors)

        .model$spec$predictors <- c('')
        for (p in predictors) {
          vars <- .model$data %>% select(p) %>% names()
          vars <- stringr::str_c(vars, collapse=' | ')
          .model$spec$predictors <- c(
            .model$spec$predictors,
            vars
          )
        }
        if (!is.null(predictor_labels)) {
          names(.model$spec$predictors) <- c('Basic', predictor_labels)
        }
        if (!is.null(labels)) {
          names(.model$spec$predictors) <- c('Basic', labels)
        }
        return(.model)
      }
    )
  .model
}

#' Add  predictors to an model spec
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
add_predictors <- function(.model, ...) {
  vars <- .model$data %>% select(...)
  vars <- vars %>% names()
  vars <- stringr::str_c(vars, collapse=' | ')

  current_predictors <- .model$spec$predictors
  if (length(current_predictors) == 0) {
    .model$spec$predictors <- c(
      '',
      vars
    )
  } else {
    .model$spec$predictors <- c(
      current_predictors,
      vars
    )
  }

  .model
}


#' Get all unique predictors
#'
#' @param model abaModel. model to get predictors from
#'
#' @return character vector. all unique predictors
#' @export
#'
#' @examples
#' x <- 1
get_predictors <- function(model) {
  model$spec$predictors %>%
    purrr::map(~strsplit(.,' | ',fixed=T)) %>%
    unlist() %>% unique()
}


#' Set statistical model of an aba model
#'
#' @param .model abaModel. aba model to alter.
#' @param ... vector. Which statistical models to use.
#' @param labels vector optional labels for spec parameter - printing/plotting.
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' # create default stat object by string
#' m <- aba_model() %>% set_stats('glm')
#' # create a stat object with - useful w/ extra params
#' m <- aba_model() %>% set_stats(aba_glm())
set_stats <- function(.model, ..., labels = NULL) {
  stats <- list(...) %>%
    purrr::map(
      function(x) {
        if (is.character(x)) x <- aba_stat_lookup(x)
        return(x)
      }
    )

  # set labels
  if (!is.null(labels)) {
    names(stats) <- labels
  } else {
    names(stats) <- stats %>%
      purrr::map_chr('stat_type') %>%
      make.names(., unique=T)
  }

  .model$spec$stats <- stats
  .model
}

#' Set data element of an aba model
#'
#' @param model abaModel. model to which data will be added
#' @param data data.frames. data to add
#'
#' @return An abaModel object
#' @export
#'
#' @examples
#' m <- aba_model() %>% set_data(data.frame(x=c(1,2,3)))
set_data <- function(model, data) {
  if (!is.data.frame(data)) stop('data argument must be data.frame')

  model$data <- data
  model
}

#' Set the endpoints of an aba_trial spec
#'
#' @param .model abaTrial
#' @param ... endpoints
#'
#' @return An abaModel object
#'
#' @export
#'
#' @examples
#'x <- 1
set_treatment <- function(.model, ...) {
  .model$spec$treatment <-
    unname(unlist(parse_select_expr(..., data=.model$data)))

  if (!is.null(.model$spec$predictors)) {
    stop('Must set one of treatment of predictors')
  }
  .model <- .model %>% set_predictors(.model$spec$treatment)
  .model
}
