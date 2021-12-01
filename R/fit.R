#' @importFrom generics fit
#' @export
generics::fit


#' Generic compile method
#'
#' @param model aba-type model
#'
#' @return aba-type model
#' @export
#'
#' @examples
#' 1 == 1
compile <- function(model) {
  UseMethod('compile')
}

# compile abaModel
#' @export
compile.abaModel <- function(model) {


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

  # remove basic models where the stat object specifices
  mr <- model$results %>%
    rowwise() %>%
    mutate(
      include_basic = ifelse(is.null(stats_obj$params$include.basic), TRUE,
                             stats_obj$params$include.basic)
    ) %>%
    filter((include_basic==TRUE) | (include_basic == FALSE & predictors != '')) %>%
    select(-include_basic)

  mr <- mr %>%
    rename(
      predictor_set = MID,
      group = groups,
      outcome = outcomes,
      predictor = predictors,
      covariate = covariates,
      stat = stats,
      stat_obj = stats_obj
    )

  model$results <- mr


  return(model)
}

# need a preprocessing function to parse

parse_then_fit_abaModel <- function(
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

process_dataset <- function(data, group, outcome, predictors, covariates, params) {
  std.beta <- params$std.beta
  complete.cases <- params$complete.cases

  data <- data %>% filter(rlang::eval_tidy(rlang::parse_expr(group)))

  # workaround for empty predictor set
  if (is.null(predictors)) return(list(data))

  predictors <- stringr::str_split(predictors, ' (\\*|\\+) ') %>%
    unlist() %>% unique() %>% subset(. != '')

  if (std.beta) {
    data[,predictors] <- scale(data[,predictors])
    data[,covariates] <- scale(data[,covariates])
  }
  if (complete.cases) {
    data <- data[complete.cases(data[,c(covariates,predictors)]),]
  }

  return(list(data))
}


#' Fit an aba model.
#'
#' This will trigger the fitting of all statistical models
#' (`stats`) on the different parameter combinations (`spec`).
#'
#' @param object abaModel. The aba model to be fitted.
#' @param ... additional parameters.
#'
#' @return abaModel
#' @export
#' @examples
#' m <- aba_model()
fit.abaModel <- function(object, ...) {
  model <- object

  # compile model
  model <- model %>% compile()

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
        predictors = model$spec$predictors,
        covariates = model$spec$covariates,
        params = model$spec$stats[[.data$stat]]$params
      )
    ) %>%
    unnest(info) %>%
    rowwise() %>%
    mutate(
      stat_fit = parse_then_fit_abaModel(
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


