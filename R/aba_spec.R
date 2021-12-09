#' Create a aba spec object.
#'
#' An aba spec is used to create an aba model. Although it is not expected that
#' a user will create an aba spec directly, it can still be useful to understand
#' how aba models are defined and can also be useful for sharing model results
#' without having to share data or fitted models. Mostly, you will want to look
#' at the automatically generated spec from an aba model.
#'
#' @param groups vector or list of logical statements as trings. Groups are
#' subsets of the data on which different models will be fit.
#' @param outcomes vector or list of strings Outcomes are the dependent
#'   variables in the statistical fits.
#' @param covariates vector of strings Covariates are independent variables
#'   which remain fixed across all statistical fits and are therefore always
#'   included with the different combinations of predictors.
#' @param predictors vector or list of strings Predictors are independent
#'   variables which you want to vary. You can include variables on their own
#'   or in combination with others. A collection of variables is referred to as
#'   a `predictor_set`.
#' @param stats string or abaStat object(s) with `stat_` prefix. Stats are
#'   the actual statistical models which you want to fit on the data. Their
#'   primary functions are to 1) generate a suitable model formula given the
#'   outcome - covariate - predictor combination, and 2) to actually fit the
#'   statistical model.
#'
#' @return An abaSpec object which can be used to create an aba model and
#'   specifies how an aba model should be fit. This aba spec object can be
#'   shared to demonstrate how an aba model should be created without having
#'   to actually share any data or actual fits.
#'
#' @export
#'
#' @examples
#'
#' # create an aba model
#' model <- aba_model() %>%
#'   set_data(adnimerge %>% dplyr::filter(VISCODE == 'bl')) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' # extract model spec... not super useful in its raw form
#' model_spec <- model$spec
#'
aba_spec <- function(groups = NULL,
                     outcomes = NULL,
                     covariates = NULL,
                     predictors = NULL,
                     stats = NULL) {

  if (is.null(groups)) groups <- 'everyone()'

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


#' Set the groups of an aba model.
#'
#' Groups are the filtered subsets of data which you want to fit statistical
#' models on. This function supports both string
#' inputs and logical functions of variables (provided that the data is already
#' set for the aba model). The inputs should be separated
#' by a comma, where each input is a different group. You can also specify
#' labels for each group.
#'
#' Note that `everyone()` or `"everyone()"` can be used to specify a group with
#' no filtering. This can be useful when you want to fit models on the entire
#' group and on a sub-group.
#'
#' @param object An aba model. The model for which you want to set groups.
#' @param ... comma-separated strings or logical expressions. This specifies
#'   the subsets of the data by which the aba model will filter.
#' @param labels vector of strings. Optional labels for printing & plotting.
#'
#' @return An aba model with groups set to the given input.
#'
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # set groups based on logical expressions. Here, data must be supplied first.
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl == 'CU',
#'     (DX_bl %in% c('MCI','AD')) & (CSF_ABETA_bl < 880)
#'   )
#' print(model)
#'
#' # specify labels which will be used later for printing & plotting
#' model <- data %>% aba_model() %>%
#'   set_groups(
#'     everyone(),
#'     DX_bl == 'CU',
#'     (DX_bl %in% c('MCI','AD')) & (CSF_ABETA_bl < 880),
#'     labels = c('All participants', 'CU-only', 'Ab+ MCI & AD')
#'   )
#' print(model)
#'
#' # set groups based on strings. No data is required to be supplied first.
#' model <- aba_model() %>%
#'   set_groups(
#'     "everyone()",
#'     "DX_bl == 'CU'",
#'     "(DX_bl %in% c('MCI','AD')) & (CSF_ABETA_bl < 880)"
#'   )
#' print(model)
#'
set_groups <- function(object, ..., labels = NULL) {
  object$spec$groups <-
    unname(unlist(parse_filter_expr(..., data=object$data)))

  # set labels
  if (!is.null(labels)) {
    names(object$spec$groups) <- labels
  }

  object
}

#' Set the outcomes of an aba model.
#'
#' Outcomes are the dependent
#' variables of the statistical models. This function supports both string
#' inputs and actual variables as found in tidy-selection. The inputs should be
#' separated by a comma, where each input is a different outcome You can also
#' specify labels for each outcome.
#'
#' @param object An aba model. The model for which you want to set outcomes
#' @param ... strings or variables. Each comma-seperated value will be a
#'   new outcome. If you give variables, then the data of the aba model should
#'   already be set.
#' @param labels vector of strings. Optional labels for printing & plotting.
#'
#' @return An aba model with outcomes set.
#'
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # set with variables
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_outcomes(CDRSB, ADAS13, MMSE)
#'
#' # supply labels
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_outcomes(CDRSB, ADAS13, MMSE, labels=c('CDR-SB','ADAS-13','MMSE'))
#'
#' # supply strings - data does not need to be set first here. But it will
#' # result in an error if these variables do not éxist in the eventual data.
#' model <- aba_model() %>%
#'   set_outcomes('CDRSB', 'ADAS13', 'MMSE')
#'
set_outcomes <- function(object, ..., labels = NULL) {
  object$spec$outcomes <-
    unname(unlist(parse_select_expr(..., data=object$data)))

  # set labels
  if (!is.null(labels)) {
    names(object$spec$outcomes) <- labels
  }

  object
}

#' Set the covariates of an aba model.
#'
#' Covariates are the independent variables which you want to always be included
#' in your statistical models - regardless of the groups, outcomes, or
#' predictors. Only one set of covariates can be supplied. If you want to test
#' multiple sets of covariates, then you should specify them as predictors or you
#' should create a new, separate model. This function supports both string
#' inputs and actual variables. The inputs should be separated
#' by a comma, where all variables together is the single covariate set.
#'
#' @param object an aba model. The model for which you want to set covariates.
#' @param ... strings or variables. This comma-separated collection of values
#'   will become the single set of covariates. If you supply actual variables,
#'   then the data of the aba model should already be set.
#' @param labels vector of strings. Optional labels for printing & plotting.
#'
#' @return An aba model with covariates set.
#'
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # set with variables
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_covariates(AGE, GENDER, EDUCATION)
#'
#' # supply strings - data does not need to be set first here. But it will
#' # result in an error if these variables do not éxist in the eventual data.
#' model <- aba_model() %>%
#'   set_covariates('AGE', 'GENDER', 'EDUCATION')
#'
set_covariates <- function(object, ...) {
  object$spec$covariates <-
    unname(unlist(parse_select_expr(..., data=object$data)))
  object
}

#' Set the predictors of an aba model.
#'
#' Predictors are the independent variables which you want to vary as a factor
#' in your statistical models across different groups, outcomes, and stats.
#' Predictors can be supplied as individual variables or as collections of
#' variables, so we refer to a unit of predictors as a "predictor_set".
#' This function supports both string inputs and actual variables. This function
#' also supports tidy-selection functions like `contains` and `starts_with` which
#' allows convenient selection of many variables at once with common names.
#'
#' @param object An aba model. The model for which you want to set predictors
#' @param ... strings or variables or tidy-selection functions. Each
#'   comma-separated value will be a new
#'   predictor set. If you supply actual variables, then the data of the aba
#'   model should already be set.
#' @param labels vector of strings. Optional labels for printing & plotting.
#'
#' @return An aba model with predictors set.
#'
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # set with variables - this will result in four "predictor sets".
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl,
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   )
#'
#' # set with tidy selection functions - but this is only one "predictor set",
#' # not multiple individual predictor sets.
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     starts_with('PLASMA')
#'   )
#'
#' # automatically generate all possible combinations of variables
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     all_combos(c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl'))
#'   )
#'
#' # supply strings - data does not need to be set first here. But it will
#' # result in an error if these variables do not éxist in the eventual data.
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     'PLASMA_ABETA_bl',
#'     'PLASMA_PTAU181_bl',
#'     'PLASMA_NFL_bl',
#'     c('PLASMA_ABETA_bl', 'PLASMA_PTAU181_bl', 'PLASMA_NFL_bl')
#'   )
#'
set_predictors <- function(object,
                           ...,
                           labels = NULL) {
  object <-
    tryCatch(
      {
        # expect not a list input
        object$spec$predictors <- unname(
          parse_select_expr(..., data=object$data) %>%
            purrr::map_chr(~stringr::str_c(., collapse=' + '))
        )
        object$spec$predictors <- c(
          '',
          object$spec$predictors
        )
        if (!is.null(labels)) {
          names(object$spec$predictors) <- c('Basic', labels)
        }
        return(object)
      },
      error = function(cond) {
        # try with expectation of list input
        predictors <- list(...)[[1]]
        predictor_labels <- names(predictors)
        object$spec$predictors <- c('')

        for (p in predictors) {
          vars <- object$data %>% select(all_of(p)) %>% names()
          vars <- stringr::str_c(vars, collapse=' + ')
          object$spec$predictors <- c(
            object$spec$predictors,
            vars
          )
        }
        if (!is.null(predictor_labels)) {
          names(object$spec$predictors) <- c('Basic', predictor_labels)
        }
        if (!is.null(labels)) {
          names(object$spec$predictors) <- c('Basic', labels)
        }
        return(object)
      }
    )
  object
}

#' Add predictors to an aba model
#'
#' This function adds predictors to the already set predictors of an aba model.
#'
#' @param object An aba model. The model for which you want to set predictors
#' @param ... strings or variables or tidy-selection functions. Each
#'   comma-separated value will be a new
#'   predictor set. If you supply actual variables, then the data of the aba
#'   model should already be set.
#'
#' @return An aba model with predictors added.
#'
#' @export
#'
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl,
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   )
#'
#' model <- model %>%
#'   add_predictors(c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl))
#'
add_predictors <- function(object, ...) {
  vars <- object$data %>% select(...)
  vars <- vars %>% names()
  vars <- stringr::str_c(vars, collapse=' | ')

  current_predictors <- object$spec$predictors
  if (length(current_predictors) == 0) {
    object$spec$predictors <- c(
      '',
      vars
    )
  } else {
    object$spec$predictors <- c(
      current_predictors,
      vars
    )
  }

  object
}


#' Get all predictors of an aba model.
#'
#' This function can be used to get all unique predictor variables from an
#' aba model. This can be thought of as "flattening" all of the predictor
#' sets of an aba model.
#'
#' @param object an ab model. The model to get predictors from.
#'
#' @return A vector of strings containing all unique predictor variables.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model <- aba_model() %>%
#'   set_data(data) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl,
#'     PLASMA_PTAU181_bl,
#'     PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   )
#'
#' predictors <- model %>% get_predictors()
#'
get_predictors <- function(object) {
  object$spec$predictors %>%
    purrr::map(~strsplit(.,' + ',fixed=T)) %>%
    unlist() %>% unique()
}


#' Set the stats of an aba model
#'
#' Stats are the objects which specify 1) how model formulas should be created
#' from the model specification, and 2) how to actual fit statistical models.
#' Stats also have their own parameters which you can specify to change how
#' the stat is fit. Multiple stats can be specified for an aba model. The best
#' way to see all the available stats is the type `aba::stat_` in the console
#' and look at the auto-completion.
#'
#' There is a broad collection of stats implemented in aba which we plan to
#' add to. Please feel free to request more. Also, there are certain extra
#' parameters which are common to all stats. These include `std.beta` which
#' determines whether to z-score all variables prior to model fitting, and
#' `complete.cases` which determines whether to only use individuals with all
#' available data within each group - outcome but across all predictor sets.
#'
#' @param object an aba model. The model on which to set stats.
#' @param ... strings or aba stat object. Each comma-separated value will be
#'   a different stat. If you specify a string, then the default stat params
#'   will be used. Some stats require that you actually call them (e.g. `stat_lme`)
#'   because they require other parameters like `id` and `time` variables.
#' @param labels vector of strings. Labels for printing & plotting.
#'
#' @return An abaModel object with stats sets.
#'
#' @export
#'
#' @examples
#'
#' # create default stat object by specifying only a string
#' model <- aba_model() %>%
#'   set_stats('glm')
#'
#' # pass an actual stat object. This is useful to specify extra params
#' # such as `std.beta` and `complete.cases` which is common to all stats.
#' model <- aba_model() %>%
#'   set_stats(
#'     stat_glm(std.beta = T, complete.cases = F)
#'   )
#'
#' # some stats such as lme require parameters
#' # those variables are expected to exist in the eventual data
#' model <- aba_model() %>%
#'   set_stats(
#'     stat_lme(id = 'RID', time = 'YEARS_bl')
#'   )
#'
#' # you can see these extra stat params when you print the model
#' print(model)
#'
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

#' Set the data of an aba model
#'
#' The raw data will be used to fit all of the statistical models. This data
#' will be processed according to what is specified in the aba stat objects.
#'
#' @param model an aba model. The model on which data will be set.
#' @param data dataframe or tibble. The data to set.
#'
#' @return An aba model with data set.
#' @export
#'
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' # set data in the traditional way
#' model <- aba_model() %>% set_data(data)
#'
#' # pipe data into an `aba_model()` call to get access to auto-completion on
#' # variables from RStudio upon further pipes. This is useful for setting
#' # other specs because it will reduce the chance of typos on variable names.
#' model <- data %>% aba_model()
#'
set_data <- function(model, data) {
  if (!is.data.frame(data)) stop('data argument must be data.frame')

  model$data <- data
  model
}

