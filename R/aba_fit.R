

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

  # compile model
  model <- model %>% aba_compile()

  # progress bar
  pb <- NULL
  if (model$verbose) pb <- progress::progress_bar$new(total = nrow(model$results))

  fit_df <- model$results %>%
    group_by(group, outcome, stat) %>%
    nest() %>%
    rename(info = data) %>%
    rowwise() %>%
    mutate(
      data_proc = process_dataset(
        data = model$data,
        group = .data$group,
        outcome = .data$outcome,
        stat = .data$stat,
        predictors = model$spec$predictors,
        covariates = model$spec$covariates,
        params = model$spec$stats[[.data$stat]]$params
      )
    ) %>%
    unnest(info) %>%
    rowwise() %>%
    mutate(
      stat_fit = parse_then_fit(
        data = .data$data_proc,
        group = .data$group,
        outcome = .data$outcome,
        predictors = .data$predictor,
        covariates = .data$covariate,
        stat_obj = .data$stat_obj,
        pb = pb
      )
    ) %>%
    select(group, outcome, stat, predictor_set, predictor,
           covariate, stat_obj, stat_fit) %>%
    ungroup()

  model$results <- fit_df
  return(model)
}

# Generates the dataframe with all parameter combinations from a model spec.
aba_compile <- function(object, ...) {
  model <- object

  data <- model$data
  group_vals <- model$spec$group
  outcome_vals <- model$spec$outcomes
  covariate_vals <- model$spec$covariates
  predictor_vals <- model$spec$predictors
  predictor_labels <- names(model$spec$predictors)
  stat_vals <- model$spec$stats

  if (is.null(predictor_vals)) predictor_vals <- ""

  # check that minimum parameters have been set
  if (is.null(model$data)) stop('You must set data before fitting.')
  if (length(outcome_vals) == 0) stop('You must set at least one outcome.')
  if (length(predictor_vals) + length(covariate_vals) == 0) {
    stop('You must set at least one predictor or one covariate')
  }
  if (length(stat_vals) == 0) stop('You must set at least one stat.')

  val_list <- list(
    'groups' = group_vals,
    'outcomes' = as.vector(outcome_vals),
    'predictors' = as.vector(predictor_vals),
    'covariates' = stringr::str_c(covariate_vals, collapse=' + '),
    'stats' = list(stat_vals)
  )

  init_df <- val_list %>% purrr::cross_df() %>%
    group_by(groups, outcomes)

  # add model names
  if (!is.null(predictor_labels)) {
    init_df <- init_df %>%
      mutate(
        MID = predictor_labels
      ) %>%
      ungroup() %>%
      select(MID, everything())
  } else {
    init_df <- init_df %>%
      mutate(MID = paste0('M',row_number())) %>% ungroup() %>%
      select(MID, everything())
  }

  model$results <- init_df %>% tibble() %>%
    unnest_wider(stats) %>%
    pivot_longer(cols = names(stat_vals),
                 names_to = 'stats', values_to = 'stats_obj')

  model$results <- model$results %>%
    rename(
      predictor_set = MID,
      group = groups,
      outcome = outcomes,
      predictor = predictors,
      covariate = covariates,
      stat = stats,
      stat_obj = stats_obj
    )

  return(model)
}

# Makes a formula and fits the statsitical model from the given parameters.
parse_then_fit <- function(
  data, group, outcome, predictors, covariates, stat_obj, pb
) {

  if (!is.null(pb)) {
    pb$tick()
  }

  # parse predictors and covariates into vectors
  predictors <- unlist(strsplit(predictors,' \\+ '))
  covariates <- unlist(strsplit(covariates,' \\+ '))

  # fit the models
  extra_params <- stat_obj$extra_params
  my_formula <- stat_obj$formula_fn(
    outcome, predictors, covariates, extra_params
  )
  my_model <- stat_obj$fit_fn(
    my_formula, data, extra_params
  )
  return(
    list(my_model)
  )
}

# Processes the raw data from the model spec based on given parameters
process_dataset <- function(
  data, group, outcome, stat, predictors, covariates, params
) {

  std.beta <- params$std.beta
  complete.cases <- params$complete.cases

  data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(group)))

  # workaround for empty predictor set
  if (is.null(predictors)) return(list(data))

  predictors <- stringr::str_split(predictors, ' (\\*|\\+) ') %>%
    unlist() %>% unique() %>% subset(. != '')

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
      if (!(stat %in% c('glm'))) data[,outcome] <- scale(data[,outcome])
    }

  }
  if (complete.cases) {
    data <- data[complete.cases(data[,c(covariates,predictors)]),]
  }

  return(list(data))
}


