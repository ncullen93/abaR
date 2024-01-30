

#' @importFrom generics fit
#' @export
generics::fit

#' Fit an aba model.
#'
#' Calling `fit` will trigger the fitting of all statistical models which
#' have been specified for the model. This will result in fits for each
#' group - outcome - stat combination.
#'
#' Note that this function is identical to the generic `aba_fit()` function.
#'
#' @param object aba model The aba model to be fitted.
#' @param ... additional parameters.
#'
#' @return abaModel
#' @export
#' @examples
#'
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model_spec <- data %>% aba_model() %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' model <- model_spec %>% aba_fit()
#'
fit.abaModel <- function(object, ...) {
  object %>% aba_fit(...)
}

#' Fit an aba model.
#'
#' Calling `aba_fit` will trigger the fitting of all statistical models which
#' have been specified for the model. This will result in fits for each
#' group - outcome - stat combination.
#'
#' Note that this function is identical to the generic `fit()` function which
#' is also provided for compatability with the greater R ecosystem.
#'
#' @param object aba model The aba model to be fitted.
#' @param verbose logical. Whether to give a progress bar during model fitting.
#'
#' @return abaModel
#' @export
#' @examples
#' data <- adnimerge %>% dplyr::filter(VISCODE == 'bl')
#'
#' model_spec <- aba_model() %>%
#'   set_data(data) %>%
#'   set_groups(everyone()) %>%
#'   set_outcomes(ConvertedToAlzheimers, CSF_ABETA_STATUS_bl) %>%
#'   set_predictors(
#'     PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl,
#'     c(PLASMA_ABETA_bl, PLASMA_PTAU181_bl, PLASMA_NFL_bl)
#'   ) %>%
#'   set_stats('glm')
#'
#' model <- model_spec %>% aba_fit()
aba_fit <- function(object, verbose = FALSE) {
  model <- object

  # check for not fully specified model
  if (is.null(model$predictors) & is.null(model$covariates)) {
    if (!model$stats[[1]]$stat_type %in% c('retest')) {
      stop('Must set at least one of covariates or predictors before fitting.')
    }
  }
  if (is.null(model$outcomes)) stop('Must set outcomes before fitting.')
  if (is.null(model$stats)) stop('Must set stats before fitting.')
  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$evals)) model <- model %>% set_evals(eval_standard())

  # tag original data rows for future use (e.g. returning predictions)
  model$data <- model$data %>%
    mutate(.row_idx = row_number()) %>%
    select(.row_idx, everything())

  eval <- model$evals[[1]]
  model <- switch(
    eval$eval_type,
    'standard' = model %>% fit_standard(verbose = verbose),
    'boot' = model %>% fit_boot(ntrials=eval$ntrials, verbose = verbose),
    'traintest' = model %>%
      fit_traintest(split = eval$split, ntrials = eval$ntrials, verbose = verbose),
    'cv' = model %>%
      fit_cv(nfolds = eval$nfolds, ntrials = eval$ntrials, verbose = verbose)
  )

  model
}

# Generates the dataframe with all parameter combinations from a model spec.
aba_compile <- function(object, ...) {
  model <- object

  groups <- list(model$groups)
  outcomes <- list(model$outcomes)
  stats <- list(model$stats)

  predictors <- model$predictors
  if (!is.null(model$covariates)) {
    if (model$include_basic) predictors <- c(list('Basic'=c()), predictors)
  }
  # hack for stat_retest because it doesn't allow covariates or predictors..
  if (model$stats[[1]]$stat_type == 'retest') predictors <- list('Basic'=c())
  predictors <- list(predictors)

  if (is.null(model$data)) stop('You must set data before fitting.')
  if (length(outcomes) == 0) stop('You must set at least one outcome.')
  if (length(stats) == 0) stop('You must set at least one stat.')

  # covariate
  covariate_vals <- model$covariates

  val_list <- list(
    'group' = groups,
    'outcome' = outcomes,
    'stat' = stats,
    'predictor' = predictors
  )


  # create initial dataframe of the factor names
  r <- val_list %>%
    tidyr::expand_grid() %>%
    unnest_longer('group', indices_to='gid', simplify=FALSE) %>%
    unnest_longer('outcome', indices_to='oid', simplify=FALSE) %>%
    unnest_longer('stat', indices_to='sid', simplify=FALSE) %>%
    unnest_longer('predictor', indices_to='pid', simplify=FALSE) %>%
    mutate(covariate = list(covariate_vals)) %>%
    arrange('group', 'outcome', 'stat') %>%
    select(-contains('id'), everything())

  return(r)
}

# Makes a formula and fits the statsitical model from the given parameters.
fit_stat <- function(
  data, outcome, predictors, covariates, stat, pb
) {
  if (!is.null(pb)) pb$tick()

  # fit the model
  my_formula <- stat$fns$formula(
    outcome, predictors, covariates, stat$extra_params
  )
  my_model <- stat$fns$fit(my_formula, data, stat$extra_params)

  return(
    list(my_model)
  )
}

# Processes the raw data from the model spec based on given parameters
process_dataset <- function(
  data, group, outcome, stat, predictors, covariates
) {

  std.beta <- stat$params$std.beta
  complete.cases <- stat$params$complete.cases

  data <- data %>%
    filter(rlang::eval_tidy(rlang::parse_expr(group))) %>%
    drop_na(all_of(outcome))

  # process predictors and covariates; check if they exist
  predictors <- predictors %>% unlist() %>% unique()
  predictors <- predictors[predictors != '']

  # check if predictors have interaction terms
  predictors <- predictors %>%
    stringr::str_split(' \\* ') %>%
    unlist() %>%
    unique()

  has_predictors <- length(predictors) > 0
  has_covariates <- length(covariates) > 0

  if (length(std.beta) == 1) std.beta <- c(std.beta, std.beta)

  # standardize predictors and covariates
  if ((std.beta[2] == TRUE) & (has_predictors | has_covariates)) {

    # scale all continuous predictors
    if (has_predictors) {
      scale_predictors <- predictors[
        predictors %>%
          purrr::map_lgl(~class(data[[.x]]) %in% c('integer','numeric'))
      ]
      if (length(scale_predictors) > 0) {
        data[,scale_predictors] <- scale(data[,scale_predictors])
      }
    }

    # scale all continuous covariates
    if (has_covariates) {
      scale_covariates <- covariates[
        covariates %>%
          purrr::map_lgl(~class(data[[.x]]) %in% c('integer','numeric'))
      ]

      if (length(scale_covariates) > 0) {
        data[,scale_covariates] <- scale(data[,scale_covariates])
      }
    }
  }

  # standardize outcome
  if (std.beta[1] == TRUE) {
    ## scale all continuous outcomes
    if (class(data[[outcome]]) %in% c('integer', 'numeric')) {
      if (!(stat$stat_type %in% c('glm'))) {
        data[,outcome] <- scale(data[,outcome])
      }
    }
  }

  # extra variables passed directly to stat object
  extra_vars <- unname(unlist(stat$extra_params))
  if ('baseline_suffix' %in% names(stat$extra_params)) {
    blx <- stat$extra_params$baseline_suffix
    extra_vars <- c(extra_vars, glue('{outcome}_{blx}'))
  }
  extra_vars <- extra_vars[extra_vars %in% names(data)]

  # only check complete cases if there are predictors or covariates
  if (has_predictors | has_covariates) {
    # add extra variables that may be provided directly to a stat object

    check_vars <- c(covariates, predictors, extra_vars)

    # if not complete cases, take rows with at least one non-zero covariate/predictor
    if (complete.cases) {
      data <- data[complete.cases(data[,check_vars]),]
    } else {
      data <- data[rowSums(!is.na(data[,check_vars])) > 0,]
    }

    # check for empty data
    if (nrow(data) < 10) {
      message <- glue('Processed data (Group: {group} | Outcome: {outcome}) has less
                 than 10 rows. Check the following:
                 - that your group filter is valid
                 - that any your outcome(s), covariate(s), and predictor(s)
                   are not all NA in your data')
      if (complete.cases == TRUE) {
        message <- glue(
          '{message}.
        Also, try setting complete.cases = F in your stat
        e.g., model %>% set_stats(stat_glm(complete.cases=F))'
        )
      }
      stop(message)
    }
  }

  # only keep relevant variables to reduce memory
  data <- data %>%
    select(all_of(
      c('.row_idx', outcome, predictors, covariates, extra_vars)
    ))

  return(list(data))
}


