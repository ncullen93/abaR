

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
#' @param ... additional parameters.
#'
#' @return abaModel
#' @export
#' @examples
#'
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
#'
aba_fit <- function(object, ...) {
  model <- object

  if (is.null(model$groups)) model <- model %>% set_groups(everyone())
  if (is.null(model$predictors)) model$predictors <- list('Basic'=c())

  # compile model
  fit_df <- model %>% aba_compile()

  # progress bar
  pb <- NULL
  if (model$verbose) pb <- progress::progress_bar$new(total = nrow(fit_df))

  # add data
  fit_df <- fit_df %>%
    group_by(group, outcome, stat) %>%
    nest() %>%
    rename(info=data) %>%
    rowwise() %>%
    mutate(
      data = process_dataset(
        data = model$data,
        group = .data$group,
        outcome = .data$outcome,
        stat = .data$stat,
        predictors = model$predictors,
        covariates = model$covariates
      )
    ) %>%
    ungroup() %>%
    unnest(info)

  # fit model
  fit_df <- fit_df %>%
    rowwise() %>%
    mutate(
      fit = fit_stat(
        data = .data$data,
        outcome = .data$outcome,
        stat = .data$stat,
        predictors = .data$predictor,
        covariates = .data$covariate,
        pb = pb
      )
    ) %>%
    ungroup()

  # select only factor labels and fit
  fit_df <- fit_df %>%
    select(
      gid, oid, sid, pid, fit
    ) %>%
    rename(
      group = gid,
      outcome = oid,
      stat = sid,
      predictor = pid
    )

  model$results <- fit_df
  return(model)
}

# Generates the dataframe with all parameter combinations from a model spec.
aba_compile <- function(object, ...) {
  model <- object

  groups <- list(model$groups)
  outcomes <- list(model$outcomes)
  stats <- list(model$stats)
  predictors <- list(model$predictors)

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
    purrr::cross_df() %>%
    unnest_longer(group, indices_to='gid', simplify=F) %>%
    unnest_longer(outcome, indices_to='oid', simplify=F) %>%
    unnest_longer(stat, indices_to='sid', simplify=F) %>%
    unnest_longer(predictor, indices_to='pid', simplify=F) %>%
    mutate(
      covariate = list(covariate_vals)
    ) %>%
    arrange(
      group, outcome, stat
    ) %>%
    select(-contains('id'), everything())

  return(r)
}

# Makes a formula and fits the statsitical model from the given parameters.
fit_stat <- function(
  data, outcome, predictors, covariates, stat, pb
) {

  if (!is.null(pb)) {
    pb$tick()
  }
  # fit the models
  extra_params <- stat$extra_params
  my_formula <- stat$formula_fn(
    outcome, predictors, covariates, extra_params
  )

  my_model <- stat$fit_fn(
    my_formula, data, extra_params
  )
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

  data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(group)))

  # workaround for empty predictor set
  if (is.null(predictors)) return(list(data))

  predictors <- predictors %>% unlist() %>% unique()
  predictors <- predictors[predictors != '']

  if (std.beta) {

    # scale all continuous predictors
    scale_predictors <- predictors[
      predictors %>%
        purrr::map_lgl(~class(data[[.x]]) %in% c('integer','numeric'))
    ]
    if (length(scale_predictors) > 0) {
      data[,scale_predictors] <- scale(data[,scale_predictors])
    }

    # scale all continuous covariates
    scale_covariates <- covariates[
      covariates %>%
        purrr::map_lgl(~class(data[[.x]]) %in% c('integer','numeric'))
    ]

    if (length(scale_covariates) > 0) {
      data[,scale_covariates] <- scale(data[,scale_covariates])
    }

    ## scale all continuous outcomes
    if (class(data[[outcome]]) %in% c('integer', 'numeric')) {
      if (!(stat$stat_type %in% c('glm'))) {
        data[,outcome] <- scale(data[,outcome])
      }
    }

  }
  if (complete.cases) {
    data <- data[complete.cases(data[,c(covariates,predictors)]),]
  }

  return(list(data))
}


